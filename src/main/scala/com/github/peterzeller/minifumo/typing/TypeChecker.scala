package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.typing.TypedAst.*

import scala.collection.mutable.ListBuffer
import com.github.peterzeller.minifumo.common.MinifumoError

object TypeChecker:
  final case class TypeError(message: String, source: ast.SourceRange) extends MinifumoError

  final case class DataType(name: String, typeParams: List[String], ctors: List[CtorSymbol])

  // Stores member signatures without creating function symbols during the export pass.
  final case class TypeClassMemberSig(
      name: String,
      typeParams: List[String],
      params: List[Type],
      result: Type
    )

  // Typeclass definitions are tracked in the type checker export environment.
  final case class TypeClassDef(name: String, typeParams: List[String], members: List[TypeClassMemberSig])

  final case class InstanceDef(
      symbol: InstanceSymbol,
      typeParams: List[String],
      head: Type,
      givenTypes: List[Type],
      members: Map[String, ast.TopLevel.FunDecl]
    )

  final case class ExportEnv(
      functions: Map[String, FunctionSymbol],
      ctors: Map[String, CtorSymbol],
      types: Map[String, DataType],
      typeClasses: Map[String, TypeClassDef],
      instances: Map[String, InstanceDef],
      // Member name -> typeclasses that declare it (used for implicit member resolution).
      memberIndex: Map[String, List[TypeClassDef]]
    )

  val emptyExportEnv: ExportEnv =
    ExportEnv(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  final case class TypeEnv(
      scopes: List[Map[String, TermSymbol]],
      exports: ExportEnv,
      typeParams: Set[String],
      expectedReturn: Type,
      givens: List[ParamSymbol],
      substitutions: Map[Int, Type],
      nextMetaId: Int
    ):
    // Adds a new term binding to the innermost scope.
    def withBinding(symbol: TermSymbol): TypeEnv =
      scopes match
        case head :: tail => copy(scopes = (head + (symbol.name -> symbol)) :: tail)
        case Nil => copy(scopes = List(Map(symbol.name -> symbol)))

    // Looks up a term symbol in the current scope stack.
    def resolveLocal(name: String): Option[TermSymbol] =
      scopes.collectFirst { case scope if scope.contains(name) => scope(name) }

    // Creates a fresh meta type variable for unification.
    def freshMeta(): (TypeEnv, Type) =
      (copy(nextMetaId = nextMetaId + 1), Type.Meta(nextMetaId))

  private def errorAt(source: ast.SourceRange, message: String): TypeError =
    TypeError(message, source)

  private final class IdSupply:
    private var nextId: Int = 1
    def freshId(): Int =
      val id = nextId
      nextId += 1
      id

  private val builtinTypeNames: Set[String] =
    Set("Set", "Map", "unit")

  private val baseTypes: Map[String, Type] =
    Map(
      "unit" -> Type.Name("unit")
    )

  private val baseValues: Map[String, BuiltinValueSymbol] =
    Map(
      "unit" -> BuiltinValueSymbol("unit", baseTypes("unit")),
      "undefined" -> BuiltinValueSymbol("undefined", Type.Unknown)
    )

  // Merges two export environments, preferring entries from the override environment.
  def mergeExports(base: ExportEnv, overrideEnv: ExportEnv): ExportEnv =
    val merged = ExportEnv(
      functions = base.functions ++ overrideEnv.functions,
      ctors = base.ctors ++ overrideEnv.ctors,
      types = base.types ++ overrideEnv.types,
      typeClasses = base.typeClasses ++ overrideEnv.typeClasses,
      instances = base.instances ++ overrideEnv.instances,
      memberIndex = Map.empty
    )
    val memberIndex = merged.typeClasses.values
      .flatMap(tc => tc.members.map(_.name).distinct.map(_ -> tc))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toList)
      .toMap
    merged.copy(memberIndex = memberIndex)

  // Adds the standard library exports to an import environment.
  def withStandardExports(importedExports: ExportEnv): ExportEnv =
    mergeExports(Standard.standardExports, importedExports)

  // Type checks a program, including standard library exports by default.
  def checkProgram(program: ast.ProgramFile, importedExports: ExportEnv = Standard.standardExports): (Program, List[TypeError]) =
    checkProgramInternal(program, importedExports, includeStandard = true)

  // Type checks a program without automatically adding the standard library.
  def checkProgramWithoutStandard(
      program: ast.ProgramFile,
      importedExports: ExportEnv = emptyExportEnv
    ): (Program, List[TypeError]) =
    checkProgramInternal(program, importedExports, includeStandard = false)

  // Shared implementation for type checking with optional standard library.
  private def checkProgramInternal(
      program: ast.ProgramFile,
      importedExports: ExportEnv,
      includeStandard: Boolean
    ): (Program, List[TypeError]) =
    val combinedExports =
      if includeStandard then withStandardExports(importedExports) else importedExports
    val shadowedTypes = if includeStandard then Standard.standardExports.types.keySet else Set.empty
    val shadowedCtors = if includeStandard then Standard.standardExports.ctors.keySet else Set.empty
    val shadowedTypeClasses = if includeStandard then Standard.standardExports.typeClasses.keySet else Set.empty
    val (exports, exportErrors) =
      extractExports(
        program,
        combinedExports,
        includeNonExported = true,
        shadowedTypes = shadowedTypes,
        shadowedCtors = shadowedCtors,
        shadowedTypeClasses = shadowedTypeClasses
      )
    val errors = ListBuffer.empty[TypeError]
    errors ++= exportErrors
    val typedItems = program.items.map {
      case funDecl: ast.TopLevel.FunDecl =>
        val (typedFun, funErrors) = typeFunction(funDecl, exports)
        errors ++= funErrors
        typedFun
      case dataDecl: ast.TopLevel.DataDecl =>
        val ctorDecls = dataDecl.ctors.map { ctor =>
          val symbol = exports.ctors(ctor.name)
          CtorDecl(
            symbol,
            ctor.fields.map(f => CtorField(f.name, fromAstType(f.tpe))(f.source))
          )(ctor.source)
        }
        TopLevel.DataDecl(dataDecl.name, dataDecl.typeParams, ctorDecls)(dataDecl.source)
      case typeClassDecl: ast.TopLevel.TypeClassDecl =>
        val members = typeClassDecl.members.map { member =>
          TypeClassMember(
            member.name,
            member.typeParams,
            member.params.map(p => fromAstType(p.tpe)),
            member.returnType.map(fromAstType).getOrElse(Type.Unknown)
          )(member.source)
        }
        TopLevel.TypeClassDecl(typeClassDecl.name, typeClassDecl.typeParams, members)(typeClassDecl.source)
      case instanceDecl: ast.TopLevel.InstanceDecl =>
        val (typedInstance, instanceErrors) = typeInstance(instanceDecl, exports)
        errors ++= instanceErrors
        typedInstance
    }
    (Program(typedItems)(program.source), errors.toList)

  // Extracts exported symbols from a program, optionally merging base exports for imports.
  def extractExports(
      program: ast.ProgramFile,
      baseExports: ExportEnv = emptyExportEnv,
      includeNonExported: Boolean = true,
      shadowedTypes: Set[String] = Set.empty,
      shadowedCtors: Set[String] = Set.empty,
      shadowedTypeClasses: Set[String] = Set.empty
    ): (ExportEnv, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    var functions = baseExports.functions
    var ctors = baseExports.ctors
    var types = baseExports.types
    var typeClasses = baseExports.typeClasses
    var instances = baseExports.instances
    var memberIndex = baseExports.memberIndex
    val localTypes = scala.collection.mutable.Set.empty[String]
    val localCtors = scala.collection.mutable.Set.empty[String]
    val localTypeClasses = scala.collection.mutable.Set.empty[String]

    for item <- program.items do {
      item match
      case ast.TopLevel.DataDecl(name, typeParams, ctorDecls, exported) if includeNonExported || exported =>
        val isShadowedType = shadowedTypes.contains(name)
        if localTypes.contains(name) then
          errors += errorAt(item.source, s"Duplicate data type: $name")
        else if types.contains(name) && !isShadowedType then
          errors += errorAt(item.source, s"Duplicate data type: $name")
        val typeParamsTypes = typeParams.map(Type.Name.apply)
        val resultType =
          if typeParamsTypes.isEmpty then Type.Name(name) else Type.App(Type.Name(name), typeParamsTypes)
        val ctorSymbols = ctorDecls.map { ctor =>
          val fieldTypes = ctor.fields.map(f => fromAstType(f.tpe))
          val ctorType: Type.Fun = Type.Fun(fieldTypes, resultType)
          CtorSymbol(ctor.name, typeParams, ctorType, fieldTypes.length, resultType)
        }
        val ctorByName = ctorSymbols.groupBy(_.name)
        ctorByName.collect { case (ctorName, syms) if syms.length > 1 =>
          val duplicateDecls = ctorDecls.filter(_.name == ctorName)
          duplicateDecls.foreach { decl =>
            errors += errorAt(decl.source, s"Duplicate constructor: $ctorName")
          }
        }
        ctorSymbols.zip(ctorDecls).foreach { case (ctorSymbol, ctorDecl) =>
          if localCtors.contains(ctorSymbol.name) then
            errors += errorAt(ctorDecl.source, s"Duplicate constructor: ${ctorSymbol.name}")
          else if ctors.contains(ctorSymbol.name) && !shadowedCtors.contains(ctorSymbol.name) then
            errors += errorAt(ctorDecl.source, s"Duplicate constructor: ${ctorSymbol.name}")
          ctors = ctors + (ctorSymbol.name -> ctorSymbol)
          localCtors += ctorSymbol.name
        }
        types = types + (name -> DataType(name, typeParams, ctorSymbols))
        localTypes += name
        ctorDecls.foreach { ctor =>
          ctor.fields.foreach { field =>
            errors ++= validateAstType(field.tpe, typeParams.toSet, ExportEnv(functions, ctors, types, typeClasses, instances, memberIndex))
          }
        }
      case funDecl @ ast.TopLevel.FunDecl(name, typeParams, params, returnType, _, _, exported) if includeNonExported || exported =>
        if functions.contains(name) then
          errors += errorAt(item.source, s"Duplicate function: $name")
        val paramTypes = params.map(p => fromAstType(p.tpe))
        val givenTypes = funGivenTypes(funDecl)
        val returnTpe = returnType.map(fromAstType).getOrElse {
          errors += errorAt(item.source, s"Missing return type for function: $name")
          Type.Unknown
        }
        val funSymbol = FunctionSymbol(name, typeParams, Type.Fun(paramTypes, returnTpe), givenTypes)
        functions = functions + (name -> funSymbol)
        val typeParamSet = typeParams.toSet
        params.foreach { param =>
          errors ++= validateAstType(param.tpe, typeParamSet, ExportEnv(functions, ctors, types, typeClasses, instances, memberIndex))
        }
        funDecl.givenParams.foreach { param =>
          errors ++= validateAstType(param.tpe, typeParamSet, ExportEnv(functions, ctors, types, typeClasses, instances, memberIndex))
        }
        returnType.foreach { tpe =>
          errors ++= validateAstType(tpe, typeParamSet, ExportEnv(functions, ctors, types, typeClasses, instances, memberIndex))
        }
      case ast.TopLevel.TypeClassDecl(name, typeParams, members, exported) if includeNonExported || exported =>
        val isShadowedTypeClass = shadowedTypeClasses.contains(name)
        if localTypeClasses.contains(name) then
          errors += errorAt(item.source, s"Duplicate typeclass: $name")
        else if typeClasses.contains(name) && !isShadowedTypeClass then
          errors += errorAt(item.source, s"Duplicate typeclass: $name")
        val memberSigs = members.map { member =>
          if member.givenParams.nonEmpty then
            errors += errorAt(member.source, s"Typeclass member ${member.name} cannot declare given parameters")
          if member.returnType.isEmpty then
            errors += errorAt(member.source, s"Missing return type for member: ${member.name}")
          TypeClassMemberSig(
            member.name,
            member.typeParams,
            member.params.map(p => fromAstType(p.tpe)),
            member.returnType.map(fromAstType).getOrElse(Type.Unknown)
          )
        }
        typeClasses = typeClasses + (name -> TypeClassDef(name, typeParams, memberSigs))
        localTypeClasses += name
        memberSigs.foreach { member =>
          val typeParamSet = (typeParams ++ member.typeParams).toSet
          member.params.foreach { param =>
            errors ++= validateTypedType(param, typeParamSet, ExportEnv(functions, ctors, types, typeClasses, instances, memberIndex), item.source)
          }
          errors ++= validateTypedType(member.result, typeParamSet, ExportEnv(functions, ctors, types, typeClasses, instances, memberIndex), item.source)
        }
      case ast.TopLevel.InstanceDecl(name, typeParams, head, givenParams, members) if includeNonExported =>
        if instances.contains(name) then
          errors += errorAt(item.source, s"Duplicate instance: $name")
        val headType = fromAstType(head)
        val givenTypes = givenParams.map(p => fromAstType(p.tpe))
        val memberMap = members.map(m => m.name -> m).toMap
        val instanceSymbol = InstanceSymbol(name, typeParams, headType, givenTypes, Map.empty)
        instances = instances + (name -> InstanceDef(instanceSymbol, typeParams, headType, givenTypes, memberMap))
      case _ =>
    }
    memberIndex = typeClasses.values
      .flatMap(tc => tc.members.map(_.name).distinct.map(_ -> tc))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toList)
      .toMap

    val updatedInstances = instances.map { case (name, instance) =>
      val (typeClassName, typeClassArgs) = instance.head match
        case Type.App(Type.Name(tcName), args) => (tcName, args)
        case Type.Name(tcName) => (tcName, Nil)
        case _ => ("", Nil)
      val typeClassMembers = typeClasses.get(typeClassName).map { tc =>
        val subst = tc.typeParams.zip(typeClassArgs).toMap
        tc.members.map { member =>
          val params = member.params.map(param => instantiateType(param, subst))
          val result = instantiateType(member.result, subst)
          val funType: Type.Fun = Type.Fun(params ++ instance.givenTypes, result)
          member.name -> FunctionSymbol(s"${instance.symbol.name}.${member.name}", member.typeParams, funType, Nil)
        }.toMap
      }.getOrElse(Map.empty[String, FunctionSymbol])
      val symbol = instance.symbol.copy(members = typeClassMembers)
      name -> instance.copy(symbol = symbol)
    }
    (ExportEnv(functions, ctors, types, typeClasses, updatedInstances, memberIndex), errors.toList)

  private def funGivenTypes(funDecl: ast.TopLevel.FunDecl): List[Type] =
    funDecl.givenParams.map(p => fromAstType(p.tpe))

  def typeFunction(
      funDecl: ast.TopLevel.FunDecl,
      exports: ExportEnv
    ): (TopLevel.FunDecl, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val idSupply = new IdSupply
    val funSymbol = exports.functions.getOrElse(
      funDecl.name,
      FunctionSymbol(funDecl.name, funDecl.typeParams, Type.Fun(Nil, Type.Unknown), Nil)
    )
    val paramsWithSource = funDecl.params.map { param =>
      (param, ParamSymbol(param.name, fromAstType(param.tpe), idSupply.freshId()))
    }
    val givenWithSource = funDecl.givenParams.map { param =>
      (param, ParamSymbol(param.name, fromAstType(param.tpe), idSupply.freshId()))
    }
    val duplicateParams = funDecl.params.groupBy(_.name).collect { case (_, ps) if ps.length > 1 => ps }
    duplicateParams.flatten.foreach { param =>
      errors += errorAt(param.source, s"Duplicate parameter: ${param.name}")
    }
    val duplicateGivens = funDecl.givenParams.groupBy(_.name).collect { case (_, ps) if ps.length > 1 => ps }
    duplicateGivens.flatten.foreach { param =>
      errors += errorAt(param.source, s"Duplicate given parameter: ${param.name}")
    }
    val givenParamNames = funDecl.givenParams.map(_.name).toSet
    funDecl.params.filter(p => givenParamNames.contains(p.name)).foreach { param =>
      errors += errorAt(param.source, s"Parameter shadows given: ${param.name}")
    }
    val params = paramsWithSource.map(_._2)
    val givens = givenWithSource.map(_._2)
    val scopeBindings = (params ++ givens).map(p => p.name -> p).toMap
    val env =
      TypeEnv(List(scopeBindings), exports, funDecl.typeParams.toSet, funSymbol.tpe.result, givens, Map.empty, 1)
    val expectedBodyType =
      if funSymbol.tpe.result == Type.Unknown then None else Some(funSymbol.tpe.result)
    val (typedBody, bodyErrors, _) = typeSuite(funDecl.body, env, funSymbol.tpe.result, expectedBodyType, idSupply)
    errors ++= bodyErrors
    val typedFun: TopLevel.FunDecl =
      TopLevel.FunDecl(funSymbol, funDecl.typeParams, params, givens, typedBody)(funDecl.source)
    (typedFun, errors.toList)

  def typeInstance(
      instanceDecl: ast.TopLevel.InstanceDecl,
      exports: ExportEnv
    ): (TopLevel.InstanceDecl, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val instanceDef = exports.instances.get(instanceDecl.name)
    val idSupply = new IdSupply
    errors ++= validateAstType(instanceDecl.head, instanceDecl.typeParams.toSet, exports)
    val headType = fromAstType(instanceDecl.head)
    instanceDecl.givenParams.foreach { param =>
      errors ++= validateAstType(param.tpe, instanceDecl.typeParams.toSet, exports)
    }
    val (typeClassName, typeClassArgs) = headType match
      case Type.App(Type.Name(tcName), args) => (tcName, args)
      case Type.Name(tcName) => (tcName, Nil)
      case _ => ("", Nil)
    val typeClassDefOpt = exports.typeClasses.get(typeClassName)
    typeClassDefOpt match
      case None =>
        errors += errorAt(instanceDecl.source, s"Unknown typeclass: ${typeClassName}")
      case Some(typeClassDef) =>
        val duplicateMembers = instanceDecl.members.groupBy(_.name).collect { case (_, ms) if ms.length > 1 => ms }
        duplicateMembers.flatten.foreach { member =>
          errors += errorAt(member.source, s"Duplicate member ${member.name} in instance ${instanceDecl.name}")
        }
        if typeClassDef.typeParams.length != typeClassArgs.length then
          errors += errorAt(
            instanceDecl.source,
            s"Typeclass ${typeClassDef.name} expects ${typeClassDef.typeParams.length} type arguments, got ${typeClassArgs.length}"
          )
        val typeClassSubst = typeClassDef.typeParams.zip(typeClassArgs).toMap
        val membersByName = instanceDecl.members.map(m => m.name -> m).toMap
        typeClassDef.members.foreach { member =>
          membersByName.get(member.name) match
            case None =>
              errors += errorAt(instanceDecl.source, s"Missing member ${member.name} in instance ${instanceDecl.name}")
            case Some(funDecl) =>
              val expectedParams = member.params.map(param => instantiateType(param, typeClassSubst))
              val expectedResult = instantiateType(member.result, typeClassSubst)
              if funDecl.params.length != expectedParams.length then
                errors += errorAt(
                  funDecl.source,
                  s"Member ${member.name} expects ${expectedParams.length} parameters, got ${funDecl.params.length}"
                )
              funDecl.params.zip(expectedParams).foreach { case (param, expected) =>
                val actual = fromAstType(param.tpe)
                if !isCompatible(expected, actual) then
                  errors += errorAt(
                    param.source,
                    s"Member ${member.name} parameter ${param.name} has type ${renderType(actual)}, expected ${renderType(expected)}"
                  )
              }
              funDecl.returnType.foreach { actualReturn =>
                val actual = fromAstType(actualReturn)
                if !isCompatible(expectedResult, actual) then
                  errors += errorAt(
                    actualReturn.source,
                    s"Member ${member.name} returns ${renderType(actual)}, expected ${renderType(expectedResult)}"
                  )
              }
              if funDecl.returnType.isEmpty then
                errors += errorAt(funDecl.source, s"Missing return type for member: ${member.name}")
        }
    val givenParams = instanceDecl.givenParams.map { param =>
      ParamSymbol(param.name, fromAstType(param.tpe), idSupply.freshId())
    }
    val typedMembers = instanceDecl.members.flatMap { memberDecl =>
      if memberDecl.givenParams.nonEmpty then
        errors += errorAt(memberDecl.source, s"Member ${memberDecl.name} cannot declare its own given parameters")
        None
      else
        val explicitParams = memberDecl.params.map { param =>
          ParamSymbol(param.name, fromAstType(param.tpe), idSupply.freshId())
        }
        val duplicateParams = memberDecl.params.groupBy(_.name).collect { case (_, ps) if ps.length > 1 => ps }
        duplicateParams.flatten.foreach { param =>
          errors += errorAt(param.source, s"Duplicate parameter: ${param.name}")
        }
        val givenParamNames = givenParams.map(_.name).toSet
        memberDecl.params.filter(p => givenParamNames.contains(p.name)).foreach { param =>
          errors += errorAt(param.source, s"Parameter shadows instance given: ${param.name}")
        }
        val memberSymbol = instanceDef.flatMap(_.symbol.members.get(memberDecl.name)).getOrElse {
          FunctionSymbol(s"${instanceDecl.name}.${memberDecl.name}", memberDecl.typeParams, Type.Fun(Nil, Type.Unknown), Nil)
        }
        val allParams = explicitParams ++ givenParams
        val scopeBindings = allParams.map(p => p.name -> p).toMap
        val env =
          TypeEnv(
            List(scopeBindings),
            exports,
            (instanceDecl.typeParams ++ memberDecl.typeParams).toSet,
            memberSymbol.tpe.result,
            givenParams,
            Map.empty,
            1
          )
        val expectedBodyType =
          if memberSymbol.tpe.result == Type.Unknown then None else Some(memberSymbol.tpe.result)
        val (typedBody, bodyErrors, _) =
          typeSuite(memberDecl.body, env, memberSymbol.tpe.result, expectedBodyType, idSupply)
        errors ++= bodyErrors
        Some(InstanceMember(memberDecl.name, memberSymbol, allParams, typedBody)(memberDecl.source))
    }
    val instanceSymbol = instanceDef.map(_.symbol).getOrElse(
      InstanceSymbol(instanceDecl.name, instanceDecl.typeParams, headType, givenParams.map(_.tpe), Map.empty)
    )
    (TopLevel.InstanceDecl(instanceSymbol, instanceDecl.typeParams, givenParams, typedMembers)(instanceDecl.source), errors.toList)

  private def typeSuite(
      suite: ast.Suite,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (Suite, List[TypeError], TypeEnv) =
    suite match
      case ast.Suite.Single(expr) =>
        val (typedExpr, errors, updatedEnv) = typeExpr(expr, env, expectedReturn, expectedType, idSupply)
        (Suite.Single(typedExpr)(suite.source), errors, updatedEnv)
      case ast.Suite.Block(exprs) =>
        val errors = ListBuffer.empty[TypeError]
        val lastIndex = exprs.length - 1
        var currentEnv = env
        val typedExprs = exprs.zipWithIndex.map { case (expr, index) =>
          val exprExpectedType = if index == lastIndex then expectedType else None
          val (typedExpr, exprErrors, updatedEnv) =
            typeExprWithEnv(expr, currentEnv, expectedReturn, exprExpectedType, idSupply)
          errors ++= exprErrors
          currentEnv = updatedEnv
          typedExpr
        }
        val tpe = typedExprs.lastOption.map(_.tpe).getOrElse(baseTypes("unit"))
        (Suite.Block(typedExprs, tpe)(suite.source), errors.toList, currentEnv)

  private def typeExpr(
      expr: ast.Expr,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    expr match
      case ast.Expr.Return(valueExpr) =>
        val expected = expectedType.getOrElse(expectedReturn)
        checkReturn(valueExpr, expected, expr.source, env, expectedReturn, idSupply)
      case _ =>
        expectedType match
          case Some(tpe) => checkExpr(expr, env, expectedReturn, tpe, idSupply)
          case None => synthesizeExpr(expr, env, idSupply)

  // Bidirectional typing (Dunfield & Krishnaswami, 2013) splits typing into two modes:
  //   * synthesizing a type from an expression (⇒), and
  //   * checking an expression against an expected type (⇐).
  // The functions below follow that structure, with per-expression rules documented in a Pierce-style
  // notation. Expected types are passed downward to improve error localization and guide inference.

  private def synthesizeExpr(
      expr: ast.Expr,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    expr match
      case ast.Expr.Lit(value) => synthLit(value, expr.source, env)
      case ast.Expr.Var(name) => synthVar(name, expr.source, env)
      case ast.Expr.Paren(inner) => synthParen(inner, expr.source, env, idSupply)
      case ast.Expr.Block(exprs) => synthBlock(exprs, expr.source, env, idSupply)
      case ast.Expr.Call(callee, typeArgs, args, usingArgs) =>
        synthCall(callee, typeArgs, args, usingArgs, expr.source, env, idSupply)
      case ast.Expr.LetIn(name, isConstant, declaredTypeAst, valueExpr, bodyExpr) =>
        synthLetIn(name, isConstant, declaredTypeAst, valueExpr, bodyExpr, expr.source, env, idSupply)
      case ast.Expr.Bind(name, isConstant, declaredTypeAst, valueExpr) =>
        synthBind(name, isConstant, declaredTypeAst, valueExpr, expr.source, env, idSupply)
      case ast.Expr.Assign(name, valueExpr) =>
        synthAssign(name, valueExpr, expr.source, env, idSupply)
      case ast.Expr.IfThenElse(cond, thenExpr, elseExpr) =>
        synthIfThenElse(cond, thenExpr, elseExpr, expr.source, env, idSupply)
      case ast.Expr.For(name, inExpr, body) =>
        synthFor(name, inExpr, body, expr.source, env, idSupply)
      case ast.Expr.While(cond, body) =>
        synthWhile(cond, body, expr.source, env, idSupply)
      case ast.Expr.Match(scrutinee, cases) =>
        synthMatch(scrutinee, cases, expr.source, env, idSupply)
      case ast.Expr.Return(_) =>
        val symbol = ErrorSymbol("<return>", Type.Unknown)
        (
          Expr.Return(Expr.Var(symbol, Type.Unknown)(expr.source), Type.Unknown)(expr.source),
          List(errorAt(expr.source, "Return expression requires an expected return type")),
          env
        )

  private def checkExpr(
      expr: ast.Expr,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    expr match
      case ast.Expr.Lit(value) => checkLit(value, expectedType, expr.source, env)
      case ast.Expr.Var(name) => checkVar(name, expectedType, expr.source, env)
      case ast.Expr.Paren(inner) => checkParen(inner, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Block(exprs) =>
        checkBlock(exprs, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Call(callee, typeArgs, args, usingArgs) =>
        checkCall(callee, typeArgs, args, usingArgs, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.LetIn(name, isConstant, declaredTypeAst, valueExpr, bodyExpr) =>
        checkLetIn(
          name,
          isConstant,
          declaredTypeAst,
          valueExpr,
          bodyExpr,
          expectedType,
          expr.source,
          env,
          expectedReturn,
          idSupply
        )
      case ast.Expr.Bind(name, isConstant, declaredTypeAst, valueExpr) =>
        checkBind(name, isConstant, declaredTypeAst, valueExpr, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Assign(name, valueExpr) =>
        checkAssign(name, valueExpr, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.IfThenElse(cond, thenExpr, elseExpr) =>
        checkIfThenElse(cond, thenExpr, elseExpr, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.For(name, inExpr, body) =>
        checkFor(name, inExpr, body, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.While(cond, body) =>
        checkWhile(cond, body, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Match(scrutinee, cases) =>
        checkMatch(scrutinee, cases, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Return(valueExpr) =>
        checkReturn(valueExpr, expectedType, expr.source, env, expectedReturn, idSupply)

  // T-Lit: Γ ⊢ n ⇒ Int / Γ ⊢ true ⇒ Bool / Γ ⊢ "s" ⇒ String
  private def synthLit(
      value: ast.Literal,
      source: ast.SourceRange,
      env: TypeEnv
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val tpe = literalType(value, source, env, errors)
    (Expr.Lit(value, tpe)(source), errors.toList, env)

  // T-Lit-Check: Γ ⊢ e ⇐ T  if  Γ ⊢ e ⇒ S  and  S ≈ T
  private def checkLit(
      value: ast.Literal,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val tpe = literalType(value, source, env, errors)
    val (updatedEnv, checkedType, expectedErrors) = ensureExpectedType(tpe, Some(expectedType), source, env)
    (Expr.Lit(value, checkedType)(source), errors.toList ++ expectedErrors, updatedEnv)

  // T-Var: Γ(x) = T  ⇒  Γ ⊢ x ⇒ T
  private def synthVar(
      name: String,
      source: ast.SourceRange,
      env: TypeEnv
    ): (Expr, List[TypeError], TypeEnv) =
    resolveSymbol(name, env) match
      case Some(symbol: CtorSymbol) if symbol.arity == 0 =>
        (Expr.CallCtor(symbol, Nil, symbol.resultType)(source), Nil, env)
      case Some(symbol) =>
        (Expr.Var(symbol, symbol.tpe)(source), Nil, env)
      case None =>
        val symbol = ErrorSymbol(name, Type.Unknown)
        (Expr.Var(symbol, symbol.tpe)(source), List(errorAt(source, s"Unknown symbol: $name")), env)

  // T-Var-Check: Γ(x) = T  ⇒  Γ ⊢ x ⇐ T'
  private def checkVar(
      name: String,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv
    ): (Expr, List[TypeError], TypeEnv) =
    resolveSymbol(name, env) match
      case Some(symbol: CtorSymbol) if symbol.arity == 0 =>
        val tpe = if expectedType != Type.Unknown then
          val typeParamBindings = collectTypeParamBindings(
            symbol.resultType,
            expectedType,
            symbol.typeParams.toSet,
            Map.empty
          )
          instantiateType(symbol.resultType, typeParamBindings)
        else symbol.resultType
        val (updatedEnv, checkedType, errors) = ensureExpectedType(tpe, Some(expectedType), source, env)
        (Expr.CallCtor(symbol, Nil, checkedType)(source), errors, updatedEnv)
      case Some(symbol) =>
        val (updatedEnv, checkedType, errors) = ensureExpectedType(symbol.tpe, Some(expectedType), source, env)
        (Expr.Var(symbol, checkedType)(source), errors, updatedEnv)
      case None =>
        val symbol = ErrorSymbol(name, Type.Unknown)
        val (updatedEnv, checkedType, errors) = ensureExpectedType(symbol.tpe, Some(expectedType), source, env)
        (Expr.Var(symbol, checkedType)(source), errors :+ errorAt(source, s"Unknown symbol: $name"), updatedEnv)

  // T-Paren: Γ ⊢ e ⇒ T  ⇒  Γ ⊢ (e) ⇒ T
  private def synthParen(
      inner: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedInner, errors, updatedEnv) = synthesizeExpr(inner, env, idSupply)
    (Expr.Paren(typedInner, typedInner.tpe)(source), errors, updatedEnv)

  // T-Paren-Check: Γ ⊢ e ⇐ T  ⇒  Γ ⊢ (e) ⇐ T
  private def checkParen(
      inner: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedInner, errors, updatedEnv) = checkExpr(inner, env, expectedReturn, expectedType, idSupply)
    (Expr.Paren(typedInner, typedInner.tpe)(source), errors, updatedEnv)

  private def synthBlock(
      exprs: List[ast.Expr],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    typeBlockExpr(exprs, None, source, env, env.expectedReturn, idSupply)

  private def checkBlock(
      exprs: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    typeBlockExpr(exprs, Some(expectedType), source, env, expectedReturn, idSupply)

  private def typeBlockExpr(
      exprs: List[ast.Expr],
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val lastIndex = exprs.length - 1
    var currentEnv = env
    val typedExprs = exprs.zipWithIndex.map { case (expr, index) =>
      val exprExpectedType = if index == lastIndex then expectedType else None
      val (typedExpr, exprErrors, updatedEnv) =
        typeExprWithEnv(expr, currentEnv, expectedReturn, exprExpectedType, idSupply)
      errors ++= exprErrors
      currentEnv = updatedEnv
      typedExpr
    }
    val tpe = typedExprs.lastOption.map(_.tpe).getOrElse(baseTypes("unit"))
    (Expr.Block(typedExprs, tpe)(source), errors.toList, currentEnv)

  private def typeExprWithEnv(
      expr: ast.Expr,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    expr match
      case ast.Expr.Bind(name, isConstant, declaredTypeAst, valueExpr) =>
        val (typedExpr, errors, updatedEnv) =
          typeBindWithEnv(name, isConstant, declaredTypeAst, valueExpr, expectedType, expr.source, env, expectedReturn, idSupply)
        (typedExpr, errors, updatedEnv)
      case ast.Expr.Assign(name, valueExpr) =>
        val (typedExpr, errors, updatedEnv) = expectedType match
          case Some(tpe) => checkAssign(name, valueExpr, tpe, expr.source, env, expectedReturn, idSupply)
          case None => synthAssign(name, valueExpr, expr.source, env, idSupply)
        (typedExpr, errors, updatedEnv)
      case _ =>
        val (typedExpr, errors, updatedEnv) = expectedType match
          case Some(tpe) => checkExpr(expr, env, expectedReturn, tpe, idSupply)
          case None => synthesizeExpr(expr, env, idSupply)
        (typedExpr, errors, updatedEnv)

  // T-App: Γ ⊢ f ⇒ T1 → T2  and  Γ ⊢ a ⇐ T1  ⇒  Γ ⊢ f a ⇒ T2
  private def synthCall(
      callee: ast.Expr,
      typeArgs: List[ast.Type],
      args: List[ast.Expr],
      usingArgs: List[ast.Expr],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    checkCall(callee, typeArgs, args, usingArgs, Type.Unknown, source, env, env.expectedReturn, idSupply)

  // T-App-Check: Γ ⊢ f ⇒ T1 → T2  and  Γ ⊢ a ⇐ T1  and  T2 ≈ T  ⇒  Γ ⊢ f a ⇐ T
  private def checkCall(
      callee: ast.Expr,
      typeArgs: List[ast.Type],
      args: List[ast.Expr],
      usingArgs: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    callee match
      case ast.Expr.Var(name) if env.exports.memberIndex.contains(name) =>
        typeClassMemberCall(name, typeArgs, args, usingArgs, expectedType, source, env, expectedReturn, idSupply)
      case _ =>
        val (typedCallee, calleeErrors, envAfterCallee) = synthesizeExpr(callee, env, idSupply)
        typedCallee match
          case Expr.Var(symbol, _) if symbol.name == "." && args.length == 2 =>
            val (typedArgs, argsErrors, envAfterArgs) =
              typeExprs(args, envAfterCallee, expectedReturn, None, idSupply)
            val errors = calleeErrors ++ argsErrors :+ errorAt(callee.source, "Field access typing is not implemented")
            (Expr.CallFun(typedCallee, typedArgs, Nil, Type.Unknown)(source), errors, envAfterArgs)
          case Expr.Var(symbol: CtorSymbol, _) =>
            val (typedExpr, ctorErrors, envAfterCtor) =
              typeCtorCall(symbol, args, expectedType, source, envAfterCallee, expectedReturn, idSupply)
            (typedExpr, calleeErrors ++ ctorErrors, envAfterCtor)
          case _ =>
            val maybeFunType = resolveFunctionType(typedCallee, envAfterCallee)
            maybeFunType match
              case Some((funType, typeParams, givenTypes)) =>
                val errors = ListBuffer.empty[TypeError]
                errors ++= calleeErrors
                val (envAfterTypeParams, typeParamBindings, typeArgErrors) =
                  instantiateTypeParams(typeParams, typeArgs, envAfterCallee, source)
                errors ++= typeArgErrors
                val instantiatedFunType: Type.Fun = instantiateType(funType, typeParamBindings) match
                  case fun: Type.Fun => fun
                  case _ => Type.Fun(Nil, Type.Unknown)
                val (envAfterResult, unifiedResult, resultErrors) =
                  if expectedType != Type.Unknown then
                    unifyTypes(instantiatedFunType.result, expectedType, envAfterTypeParams, source, "call result")
                  else
                    (envAfterTypeParams, instantiatedFunType.result, Nil)
                errors ++= resultErrors
                var currentEnv = envAfterResult
                val typedArgs = args.zipWithIndex.map { case (arg, index) =>
                  val expectedParamType =
                    if index < instantiatedFunType.params.length then
                      applySubstitutions(instantiatedFunType.params(index), currentEnv)
                    else Type.Unknown
                  val (typedArg, argErrors, updatedEnv) =
                    checkExpr(arg, currentEnv, expectedReturn, expectedParamType, idSupply)
                  errors ++= argErrors
                  currentEnv = updatedEnv
                  typedArg
                }
                if instantiatedFunType.params.length != args.length then
                  errors += errorAt(
                    callee.source,
                    s"Call expects ${instantiatedFunType.params.length} args, got ${args.length}"
                  )
                val instantiatedGivenTypes = givenTypes.map(tpe => instantiateType(tpe, typeParamBindings))
                val (givenArgsResolved, givenErrors, envAfterGiven) =
                  resolveGivenArguments(instantiatedGivenTypes, usingArgs, currentEnv, source, idSupply, Nil)
                errors ++= givenErrors
                val finalResultType = applySubstitutions(unifiedResult, envAfterGiven)
                (
                  Expr.CallFun(typedCallee, typedArgs, givenArgsResolved, finalResultType)(source),
                  errors.toList,
                  envAfterGiven
                )
              case None =>
                val (typedArgs, argsErrors, envAfterArgs) = typeExprs(args, envAfterCallee, expectedReturn, None, idSupply)
                val errors =
                  calleeErrors ++ argsErrors :+ errorAt(
                    callee.source,
                    s"Call target is not a function: ${renderType(typedCallee.tpe)}"
                  )
                (Expr.CallFun(typedCallee, typedArgs, Nil, Type.Unknown)(source), errors, envAfterArgs)

  private def typeClassMemberCall(
      name: String,
      typeArgs: List[ast.Type],
      args: List[ast.Expr],
      usingArgs: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    if typeArgs.nonEmpty then
      errors += errorAt(source, s"Type arguments are not supported for typeclass member calls: $name")
    val candidates = env.exports.memberIndex.getOrElse(name, Nil).flatMap { tc =>
      tc.members.find(_.name == name).map(member => (tc, member))
    }
    val typedCandidates = candidates.flatMap { case (tc, member) =>
      val (envAfterTypeParams, typeParamBindings, typeArgErrors) =
        instantiateTypeParams(tc.typeParams ++ member.typeParams, Nil, env, source)
      val candidateErrors = ListBuffer.empty[TypeError]
      candidateErrors ++= errors ++ typeArgErrors
      val instantiatedResultType = instantiateType(member.result, typeParamBindings)
      val (envAfterResult, unifiedResult, resultErrors) =
        if expectedType != Type.Unknown then
          unifyTypes(instantiatedResultType, expectedType, envAfterTypeParams, source, "typeclass member result")
        else
          (envAfterTypeParams, instantiatedResultType, Nil)
      candidateErrors ++= resultErrors
      var currentEnv = envAfterResult
      val typedArgs = args.zipWithIndex.map { case (arg, index) =>
        val expectedParamType =
          if index < member.params.length then
            applySubstitutions(instantiateType(member.params(index), typeParamBindings), currentEnv)
          else Type.Unknown
        val (typedArg, argErrors, updatedEnv) =
          checkExpr(arg, currentEnv, expectedReturn, expectedParamType, idSupply)
        candidateErrors ++= argErrors
        currentEnv = updatedEnv
        typedArg
      }
      if member.params.length != args.length then
        candidateErrors += errorAt(source, s"Call expects ${member.params.length} args, got ${args.length}")
      val typeClassArgs = tc.typeParams.map { param =>
        typeParamBindings.getOrElse(param, Type.Unknown)
      }
      val goal = if typeClassArgs.isEmpty then Type.Name(tc.name) else Type.App(Type.Name(tc.name), typeClassArgs)
      val preferGiven =
        usingArgs.isEmpty && typedArgs.exists { arg =>
          arg.tpe match
            case Type.Name(paramName) if env.typeParams.contains(paramName) => true
            case _ => false
        }
      val (instanceArgs, instanceErrors, envAfterInstance) =
        if preferGiven then
          val matchingGivens = env.givens.filter(g => isCompatible(g.tpe, goal))
          matchingGivens match
            case param :: Nil => (List(Expr.Var(param, param.tpe)(source)), Nil, currentEnv)
            case _ :: _ => (Nil, List(errorAt(source, s"Ambiguous given for ${renderType(goal)}")), currentEnv)
            case Nil => resolveGivenArguments(List(goal), usingArgs, currentEnv, source, idSupply, Nil)
        else
          resolveGivenArguments(List(goal), usingArgs, currentEnv, source, idSupply, Nil)
      candidateErrors ++= instanceErrors
      instanceArgs.headOption.map { instanceExpr =>
        val finalResultType = applySubstitutions(unifiedResult, envAfterInstance)
        (Expr.CallTypeClassMember(instanceExpr, name, typedArgs, finalResultType)(source), candidateErrors.toList, envAfterInstance)
      }
    }
    typedCandidates.distinct match
      case Nil =>
        val missingSymbol = ErrorSymbol(name, Type.Unknown)
        val baseErrors =
          if candidates.isEmpty || errors.isEmpty then
            errors.toList :+ errorAt(source, s"No typeclass member found: $name")
          else
            errors.toList
        (Expr.Var(missingSymbol, Type.Unknown)(source), baseErrors, env)
      case (expr, candidateErrors, candidateEnv) :: Nil =>
        (expr, candidateErrors, candidateEnv)
      case _ =>
        val missingSymbol = ErrorSymbol(name, Type.Unknown)
        (
          Expr.Var(missingSymbol, Type.Unknown)(source),
          errors.toList :+ errorAt(source, s"Ambiguous typeclass member call: $name"),
          env
        )

  // Instantiates type parameters with explicit arguments or fresh meta variables.
  private def instantiateTypeParams(
      typeParams: List[String],
      typeArgs: List[ast.Type],
      env: TypeEnv,
      source: ast.SourceRange
    ): (TypeEnv, Map[String, Type], List[TypeError]) =
    if typeArgs.nonEmpty then
      if typeParams.length != typeArgs.length then
        (
          env,
          typeParams.map(_ -> Type.Unknown).toMap,
          List(errorAt(source, s"Expected ${typeParams.length} type arguments, got ${typeArgs.length}"))
        )
      else
        (env, typeParams.zip(typeArgs.map(fromAstType)).toMap, Nil)
    else
      val initial = (env, Map.empty[String, Type])
      val (updatedEnv, bindings) = typeParams.foldLeft(initial) { case ((currentEnv, acc), name) =>
        val (nextEnv, meta) = currentEnv.freshMeta()
        (nextEnv, acc + (name -> meta))
      }
      (updatedEnv, bindings, Nil)

  private def resolveGivenArguments(
      givenTypes: List[Type],
      usingArgs: List[ast.Expr],
      env: TypeEnv,
      source: ast.SourceRange,
      idSupply: IdSupply,
      stack: List[Type]
    ): (List[Expr], List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    if usingArgs.nonEmpty && usingArgs.length != givenTypes.length then
      errors += errorAt(source, s"Expected ${givenTypes.length} using arguments, got ${usingArgs.length}")
    var currentEnv = env
    val resolved = givenTypes.zipWithIndex.flatMap { case (givenType, index) =>
      val instantiatedGoal = applySubstitutions(givenType, currentEnv)
      val (exprOpt, exprErrors, updatedEnv) =
        if usingArgs.nonEmpty && index < usingArgs.length then
          resolveUsingExpr(usingArgs(index), instantiatedGoal, currentEnv, source, idSupply)
        else
          resolveInstance(instantiatedGoal, currentEnv, source, idSupply, stack, None)
      errors ++= exprErrors
      currentEnv = updatedEnv
      exprOpt
    }
    (resolved, errors.toList, currentEnv)

  private def resolveUsingExpr(
      usingExpr: ast.Expr,
      goal: Type,
      env: TypeEnv,
      source: ast.SourceRange,
      idSupply: IdSupply
    ): (Option[Expr], List[TypeError], TypeEnv) =
    usingExpr match
      case ast.Expr.Var(name) =>
        resolveSymbol(name, env) match
          case Some(instanceSymbol: InstanceSymbol) =>
            env.exports.instances.get(instanceSymbol.name) match
              case Some(instance) => resolveNamedInstance(instance, goal, env, source, idSupply)
              case None => (None, List(errorAt(source, s"Unknown using argument: $name")), env)
          case _ =>
            val (typedExpr, exprErrors, updatedEnv) = checkExpr(usingExpr, env, env.expectedReturn, goal, idSupply)
            (Some(typedExpr), exprErrors, updatedEnv)
      case _ =>
        val (typedExpr, exprErrors, updatedEnv) = checkExpr(usingExpr, env, env.expectedReturn, goal, idSupply)
        (Some(typedExpr), exprErrors, updatedEnv)

  private def resolveNamedInstance(
      instance: InstanceDef,
      goal: Type,
      env: TypeEnv,
      source: ast.SourceRange,
      idSupply: IdSupply
    ): (Option[Expr], List[TypeError], TypeEnv) =
    val (bindingsOpt, matchErrors) = matchInstance(instance, goal, source)
    bindingsOpt match
      case None => (None, matchErrors, env)
      case Some((bindings, instantiatedHead)) =>
        val prunedBindings = dropSelfBindings(bindings)
        val (givenArgs, givenErrors, updatedEnv) =
          resolveGivenArguments(
            instance.givenTypes.map(tpe => instantiateType(tpe, prunedBindings)),
            Nil,
            env,
            source,
            idSupply,
            goal :: Nil
          )
        val expr = Expr.InstanceValue(instance.symbol, givenArgs, instantiatedHead)(source)
        (Some(expr), matchErrors ++ givenErrors, updatedEnv)

  private def resolveInstance(
      goal: Type,
      env: TypeEnv,
      source: ast.SourceRange,
      idSupply: IdSupply,
      stack: List[Type],
      filter: Option[Type]
    ): (Option[Expr], List[TypeError], TypeEnv) =
    if stack.exists(t => t == goal) then
      (None, List(errorAt(source, s"Cyclic instance resolution for ${renderType(goal)}")), env)
    else
      val errors = ListBuffer.empty[TypeError]
      val givenMatches = env.givens.filter(g => isCompatible(g.tpe, goal))
      val filteredGiven = filter match
        case Some(f) => givenMatches.filter(g => isCompatible(g.tpe, f))
        case None => givenMatches
      filteredGiven match
        case param :: Nil =>
          return (Some(Expr.Var(param, param.tpe)(source)), Nil, env)
        case _ :: _ =>
          return (None, List(errorAt(source, s"Ambiguous given for ${renderType(goal)}")), env)
        case Nil => ()
      val instanceCandidates = env.exports.instances.values.toList.filter { inst =>
        filter.forall(f => isCompatible(inst.head, f)) && matchInstanceHead(inst, goal)
      }
      val filteredCandidates =
        if instanceCandidates.nonEmpty then
          val minTypeParams = instanceCandidates.map(_.typeParams.length).min
          instanceCandidates.filter(_.typeParams.length == minTypeParams)
        else
          instanceCandidates
      val resolved = filteredCandidates.flatMap { inst =>
        val (bindingsOpt, matchErrors) = matchInstance(inst, goal, source)
        bindingsOpt.flatMap { case (bindings, instantiatedHead) =>
          val prunedBindings = dropSelfBindings(bindings)
          val (givenArgs, givenErrors, updatedEnv) =
            resolveGivenArguments(
              inst.givenTypes.map(tpe => instantiateType(tpe, prunedBindings)),
              Nil,
              env,
              source,
              idSupply,
              goal :: stack
            )
          if givenErrors.nonEmpty then
            errors ++= matchErrors ++ givenErrors
            None
          else
            Some((Expr.InstanceValue(inst.symbol, givenArgs, instantiatedHead)(source), matchErrors, updatedEnv))
        }
      }
      resolved match
        case (expr, exprErrors, updatedEnv) :: Nil =>
          errors ++= exprErrors
          (Some(expr), errors.toList, updatedEnv)
        case Nil =>
          (None, List(errorAt(source, s"No instance found for ${renderType(goal)}")), env)
        case _ =>
          throw Exception("Unreachable code: multiple resolved instances should have been caught earlier")
          //(None, List(errorAt(source, s"Ambiguous instance for ${renderType(goal)}. Possible options:\n${resolved.map(_._1).map(r => s"- ${r._1.name}").mkString("\n")}")))

  private def matchInstanceHead(instance: InstanceDef, goal: Type): Boolean =
    val typeParamBindings = collectTypeParamBindings(instance.head, goal, instance.typeParams.toSet, Map.empty)
    val instantiatedHead = instantiateType(instance.head, typeParamBindings)
    isCompatible(instantiatedHead, goal)

  private def matchInstance(
      instance: InstanceDef,
      goal: Type,
      source: ast.SourceRange
    ): (Option[(Map[String, Type], Type)], List[TypeError]) =
    val typeParamBindings = collectTypeParamBindings(instance.head, goal, instance.typeParams.toSet, Map.empty)
    val instantiatedHead = instantiateType(instance.head, typeParamBindings)
    if isCompatible(instantiatedHead, goal) then
      (Some(typeParamBindings -> instantiatedHead), Nil)
    else
      (None, List(errorAt(source, s"Instance ${instance.symbol.name} does not match ${renderType(goal)}")))

  private def typeCtorCall(
      ctor: CtorSymbol,
      args: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val (envAfterTypeParams, typeParamBindings, typeArgErrors) =
      instantiateTypeParams(ctor.typeParams, Nil, env, source)
    errors ++= typeArgErrors
    val instantiatedCtorType: Type.Fun = instantiateType(ctor.tpe, typeParamBindings) match
      case fun: Type.Fun => fun
      case _ => Type.Fun(Nil, Type.Unknown)
    val (envAfterResult, unifiedResult, resultErrors) =
      if expectedType != Type.Unknown then
        unifyTypes(instantiatedCtorType.result, expectedType, envAfterTypeParams, source, "constructor result")
      else
        (envAfterTypeParams, instantiatedCtorType.result, Nil)
    errors ++= resultErrors
    var currentEnv = envAfterResult
    val typedArgs = args.zipWithIndex.map { case (arg, index) =>
      val expectedParamType =
        if index < instantiatedCtorType.params.length then
          applySubstitutions(instantiatedCtorType.params(index), currentEnv)
        else Type.Unknown
      val (typedArg, argErrors, updatedEnv) =
        checkExpr(arg, currentEnv, expectedReturn, expectedParamType, idSupply)
      errors ++= argErrors
      currentEnv = updatedEnv
      typedArg
    }
    if instantiatedCtorType.params.length != args.length then
      errors += errorAt(
        source,
        s"Constructor ${ctor.name} expects ${instantiatedCtorType.params.length} args, got ${args.length}"
      )
    val finalResultType = applySubstitutions(unifiedResult, currentEnv)
    (Expr.CallCtor(ctor, typedArgs, finalResultType)(source), errors.toList, currentEnv)

  // T-Let: Γ ⊢ v ⇐ T1  and  Γ,x:T1 ⊢ e ⇒ T2  ⇒  Γ ⊢ let x=v in e ⇒ T2
  private def synthLetIn(
      name: String,
      isConstant: Boolean,
      declaredTypeAst: Option[ast.Type],
      valueExpr: ast.Expr,
      bodyExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val declaredType = declaredTypeAst.map(fromAstType)
    val (typedValue, valueErrors, envAfterValue) = declaredType match
      case Some(tpe) => checkExpr(valueExpr, env, env.expectedReturn, tpe, idSupply)
      case None => synthesizeExpr(valueExpr, env, idSupply)
    val typeErrors = declaredTypeAst.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val bindingType = declaredType.getOrElse(applySubstitutions(typedValue.tpe, envAfterValue))
    val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
    val (typedBody, bodyErrors, envAfterBody) = synthesizeExpr(bodyExpr, envAfterValue.withBinding(symbol), idSupply)
    val allErrors = valueErrors ++ typeErrors ++ bodyErrors
    (Expr.LetIn(symbol, isConstant, declaredType, typedValue, typedBody, typedBody.tpe)(source), allErrors, envAfterBody)

  // T-Let-Check: Γ ⊢ v ⇐ T1  and  Γ,x:T1 ⊢ e ⇐ T2  ⇒  Γ ⊢ let x=v in e ⇐ T2
  private def checkLetIn(
      name: String,
      isConstant: Boolean,
      declaredTypeAst: Option[ast.Type],
      valueExpr: ast.Expr,
      bodyExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val declaredType = declaredTypeAst.map(fromAstType)
    val (typedValue, valueErrors, envAfterValue) = declaredType match
      case Some(tpe) => checkExpr(valueExpr, env, env.expectedReturn, tpe, idSupply)
      case None => synthesizeExpr(valueExpr, env, idSupply)
    val typeErrors = declaredTypeAst.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val bindingType = declaredType.getOrElse(applySubstitutions(typedValue.tpe, envAfterValue))
    val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
    val (typedBody, bodyErrors, envAfterBody) =
      checkExpr(bodyExpr, envAfterValue.withBinding(symbol), expectedReturn, expectedType, idSupply)
    val allErrors = valueErrors ++ typeErrors ++ bodyErrors
    (Expr.LetIn(symbol, isConstant, declaredType, typedValue, typedBody, typedBody.tpe)(source), allErrors, envAfterBody)

  private def synthBind(
      name: String,
      isConstant: Boolean,
      declaredTypeAst: Option[ast.Type],
      valueExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedExpr, errors, updatedEnv) =
      typeBindWithEnv(name, isConstant, declaredTypeAst, valueExpr, Some(baseTypes("unit")), source, env, env.expectedReturn, idSupply)
    (typedExpr, errors, updatedEnv)

  private def checkBind(
      name: String,
      isConstant: Boolean,
      declaredTypeAst: Option[ast.Type],
      valueExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedExpr, errors, updatedEnv) =
      typeBindWithEnv(name, isConstant, declaredTypeAst, valueExpr, Some(expectedType), source, env, expectedReturn, idSupply)
    (typedExpr, errors, updatedEnv)

  private def typeBindWithEnv(
      name: String,
      isConstant: Boolean,
      declaredTypeAst: Option[ast.Type],
      valueExpr: ast.Expr,
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val declaredType = declaredTypeAst.map(fromAstType)
    val (typedValue, valueErrors, envAfterValue) = declaredType match
      case Some(tpe) => checkExpr(valueExpr, env, expectedReturn, tpe, idSupply)
      case None => synthesizeExpr(valueExpr, env, idSupply)
    val typeErrors = declaredTypeAst.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val bindingType = declaredType.getOrElse(applySubstitutions(typedValue.tpe, envAfterValue))
    val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
    val (updatedEnv, checkedType, expectedErrors) =
      ensureExpectedType(baseTypes("unit"), expectedType, source, envAfterValue)
    val typedExpr = Expr.Bind(symbol, isConstant, declaredType, typedValue, checkedType)(source)
    (typedExpr, valueErrors ++ typeErrors ++ expectedErrors, updatedEnv.withBinding(symbol))

  private def synthAssign(
      name: String,
      valueExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedExpr, errors, updatedEnv) = typeAssign(name, valueExpr, None, source, env, env.expectedReturn, idSupply)
    (typedExpr, errors, updatedEnv)

  private def checkAssign(
      name: String,
      valueExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedExpr, errors, updatedEnv) = typeAssign(name, valueExpr, Some(expectedType), source, env, expectedReturn, idSupply)
    (typedExpr, errors, updatedEnv)

  private def typeAssign(
      name: String,
      valueExpr: ast.Expr,
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    env.resolveLocal(name) match
      case Some(symbol: LocalSymbol) =>
        val (typedValue, valueErrors, envAfterValue) = checkExpr(valueExpr, env, expectedReturn, symbol.tpe, idSupply)
        val (updatedEnv, checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source, envAfterValue)
        (Expr.Assign(symbol, typedValue, checkedType)(source), valueErrors ++ expectedErrors, updatedEnv)
      case Some(symbol) =>
        val (typedValue, valueErrors, envAfterValue) = synthesizeExpr(valueExpr, env, idSupply)
        val (updatedEnv, checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source, envAfterValue)
        val errors = valueErrors ++ expectedErrors :+ errorAt(source, s"Cannot assign to ${symbol.name}")
        val fallbackSymbol = LocalSymbol(name, Type.Unknown, idSupply.freshId())
        (Expr.Assign(fallbackSymbol, typedValue, checkedType)(source), errors, updatedEnv)
      case None =>
        val (typedValue, valueErrors, envAfterValue) = synthesizeExpr(valueExpr, env, idSupply)
        val (updatedEnv, checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source, envAfterValue)
        val symbol = LocalSymbol(name, Type.Unknown, idSupply.freshId())
        (
          Expr.Assign(symbol, typedValue, checkedType)(source),
          valueErrors ++ expectedErrors :+ errorAt(source, s"Unknown variable: $name"),
          updatedEnv
        )

  // T-If: Γ ⊢ c ⇐ Bool  and  Γ ⊢ t ⇒ T  and  Γ ⊢ e ⇒ T  ⇒  Γ ⊢ if c then t else e ⇒ T
  private def synthIfThenElse(
      cond: ast.Expr,
      thenExpr: ast.Expr,
      elseExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val boolType = resolveTypeName("Bool", source, env, errors)
    val (typedCond, condErrors, envAfterCond) = checkExpr(cond, env, env.expectedReturn, boolType, idSupply)
    val (typedThen, thenErrors, envAfterThen) = synthesizeExpr(thenExpr, envAfterCond, idSupply)
    val (typedElse, elseErrors, envAfterElse) = synthesizeExpr(elseExpr, envAfterThen, idSupply)
    val (envAfterBranches, resultType, branchErrors) =
      unifyBranchTypes("if", typedThen.tpe, typedElse.tpe, source, envAfterElse)
    (
      Expr.IfThenElse(typedCond, typedThen, typedElse, resultType)(source),
      errors.toList ++ condErrors ++ thenErrors ++ elseErrors ++ branchErrors,
      envAfterBranches
    )

  // T-If-Check: Γ ⊢ c ⇐ Bool  and  Γ ⊢ t ⇐ T  and  Γ ⊢ e ⇐ T  ⇒  Γ ⊢ if c then t else e ⇐ T
  private def checkIfThenElse(
      cond: ast.Expr,
      thenExpr: ast.Expr,
      elseExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val boolType = resolveTypeName("Bool", source, env, errors)
    val (typedCond, condErrors, envAfterCond) = checkExpr(cond, env, expectedReturn, boolType, idSupply)
    val (typedThen, thenErrors, envAfterThen) = checkExpr(thenExpr, envAfterCond, expectedReturn, expectedType, idSupply)
    val (typedElse, elseErrors, envAfterElse) = checkExpr(elseExpr, envAfterThen, expectedReturn, expectedType, idSupply)
    val (envAfterBranches, _, branchErrors) = unifyBranchTypes("if", typedThen.tpe, typedElse.tpe, source, envAfterElse)
    (
      Expr.IfThenElse(typedCond, typedThen, typedElse, expectedType)(source),
      errors.toList ++ condErrors ++ thenErrors ++ elseErrors ++ branchErrors,
      envAfterBranches
    )

  // T-For: Γ ⊢ xs ⇒ Iterable[T]  and  Γ,x:T ⊢ e ⇒ U  ⇒  Γ ⊢ for x in xs do e ⇒ U
  private def synthFor(
      name: String,
      inExpr: ast.Expr,
      body: ast.Suite,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedIn, inErrors, envAfterIn) = synthesizeExpr(inExpr, env, idSupply)
    val (elemType, elemErrors) = iterableElementType(typedIn.tpe, typedIn.source)
    val symbol = LocalSymbol(name, elemType, idSupply.freshId())
    val (typedBody, bodyErrors, envAfterBody) =
      typeSuite(body, envAfterIn.withBinding(symbol), env.expectedReturn, None, idSupply)
    val resultType = suiteType(typedBody)
    (
      Expr.For(symbol, typedIn, typedBody, resultType)(source),
      inErrors ++ elemErrors ++ bodyErrors,
      envAfterBody
    )

  // T-For-Check: Γ ⊢ xs ⇒ Iterable[T]  and  Γ,x:T ⊢ e ⇐ U  ⇒  Γ ⊢ for x in xs do e ⇐ U
  private def checkFor(
      name: String,
      inExpr: ast.Expr,
      body: ast.Suite,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedIn, inErrors, envAfterIn) = synthesizeExpr(inExpr, env, idSupply)
    val (elemType, elemErrors) = iterableElementType(typedIn.tpe, typedIn.source)
    val symbol = LocalSymbol(name, elemType, idSupply.freshId())
    val (typedBody, bodyErrors, envAfterBody) =
      typeSuite(body, envAfterIn.withBinding(symbol), expectedReturn, Some(expectedType), idSupply)
    (
      Expr.For(symbol, typedIn, typedBody, expectedType)(source),
      inErrors ++ elemErrors ++ bodyErrors,
      envAfterBody
    )

  // T-While: Γ ⊢ c ⇐ Bool  and  Γ ⊢ e ⇒ unit  ⇒  Γ ⊢ while c do e ⇒ unit
  private def synthWhile(
      cond: ast.Expr,
      body: ast.Suite,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val boolType = resolveTypeName("Bool", source, env, errors)
    val (typedCond, condErrors, envAfterCond) = checkExpr(cond, env, env.expectedReturn, boolType, idSupply)
    val (typedBody, bodyErrors, envAfterBody) = typeSuite(body, envAfterCond, env.expectedReturn, None, idSupply)
    val (updatedEnv, checkedType, expectedErrors) =
      ensureExpectedType(baseTypes("unit"), Some(baseTypes("unit")), source, envAfterBody)
    (
      Expr.While(typedCond, typedBody, checkedType)(source),
      errors.toList ++ condErrors ++ bodyErrors ++ expectedErrors,
      updatedEnv
    )

  // T-While-Check: Γ ⊢ c ⇐ Bool  and  Γ ⊢ e ⇐ unit  ⇒  Γ ⊢ while c do e ⇐ unit
  private def checkWhile(
      cond: ast.Expr,
      body: ast.Suite,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val boolType = resolveTypeName("Bool", source, env, errors)
    val (typedCond, condErrors, envAfterCond) = checkExpr(cond, env, expectedReturn, boolType, idSupply)
    val (typedBody, bodyErrors, envAfterBody) = typeSuite(body, envAfterCond, expectedReturn, Some(baseTypes("unit")), idSupply)
    val (updatedEnv, checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), Some(expectedType), source, envAfterBody)
    (
      Expr.While(typedCond, typedBody, checkedType)(source),
      errors.toList ++ condErrors ++ bodyErrors ++ expectedErrors,
      updatedEnv
    )

  // T-Match: Γ ⊢ e ⇒ T and Γ ⊢ cases ⇒ U  ⇒  Γ ⊢ match e with cases ⇒ U
  private def synthMatch(
      scrutinee: ast.Expr,
      cases: List[ast.MatchCase],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedScrutinee, scrutineeErrors, envAfterScrutinee) = synthesizeExpr(scrutinee, env, idSupply)
    val errors = ListBuffer.empty[TypeError]
    errors ++= scrutineeErrors
    var currentEnv = envAfterScrutinee
    val typedCases = cases.map { matchCase =>
      val (typedPattern, bindings, patternErrors) =
        typePattern(matchCase.pattern, typedScrutinee.tpe, currentEnv, idSupply)
      errors ++= patternErrors
      val envWithBindings = bindings.values.foldLeft(currentEnv) { case (current, symbol) => current.withBinding(symbol) }
      val (typedBody, bodyErrors, envAfterBody) =
        typeSuite(matchCase.body, envWithBindings, env.expectedReturn, None, idSupply)
      errors ++= bodyErrors
      currentEnv = envAfterBody
      MatchCase(typedPattern, typedBody)(matchCase.source)
    }
    val caseTypes = typedCases.map(c => suiteType(c.body))
    val (envAfterCases, resultType, caseErrors) =
      caseTypes.drop(1).foldLeft((currentEnv, caseTypes.headOption.getOrElse(Type.Unknown), List.empty[TypeError])) {
        case ((envAcc, accType, accErrors), nextType) =>
          val (nextEnv, unified, nextErrors) =
            unifyTypes(accType, nextType, envAcc, source, "match cases")
          (nextEnv, unified, accErrors ++ nextErrors)
      }
    errors ++= caseErrors
    (Expr.Match(typedScrutinee, typedCases, resultType)(source), errors.toList, envAfterCases)

  // T-Match-Check: Γ ⊢ e ⇒ T and Γ ⊢ cases ⇐ U  ⇒  Γ ⊢ match e with cases ⇐ U
  private def checkMatch(
      scrutinee: ast.Expr,
      cases: List[ast.MatchCase],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedScrutinee, scrutineeErrors, envAfterScrutinee) = synthesizeExpr(scrutinee, env, idSupply)
    val errors = ListBuffer.empty[TypeError]
    errors ++= scrutineeErrors
    var currentEnv = envAfterScrutinee
    val typedCases = cases.map { matchCase =>
      val (typedPattern, bindings, patternErrors) =
        typePattern(matchCase.pattern, typedScrutinee.tpe, currentEnv, idSupply)
      errors ++= patternErrors
      val envWithBindings = bindings.values.foldLeft(currentEnv) { case (current, symbol) => current.withBinding(symbol) }
      val (typedBody, bodyErrors, envAfterBody) =
        typeSuite(matchCase.body, envWithBindings, expectedReturn, Some(expectedType), idSupply)
      errors ++= bodyErrors
      currentEnv = envAfterBody
      MatchCase(typedPattern, typedBody)(matchCase.source)
    }
    val caseTypes = typedCases.map(c => suiteType(c.body))
    val mismatchErrors = caseTypes.sliding(2).collect {
      case List(a, b) if !isCompatible(a, b) =>
        errorAt(source, s"Match case types do not agree: ${renderType(a)} vs ${renderType(b)}")
    }.toList
    errors ++= mismatchErrors
    (Expr.Match(typedScrutinee, typedCases, expectedType)(source), errors.toList, currentEnv)

  // T-Return-Check: Γ ⊢ e ⇐ Tret  ⇒  Γ ⊢ return e ⇐ Tret
  private def checkReturn(
      valueExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val returnExpectedType = if expectedReturn == Type.Unknown then expectedType else expectedReturn
    val (typedValue, valueErrors, envAfterValue) =
      checkExpr(valueExpr, env, expectedReturn, returnExpectedType, idSupply)
    val typeErrors =
      if expectedReturn == Type.Unknown then Nil
      else
        val (_, _, expectedErrors) =
          unifyTypes(expectedReturn, typedValue.tpe, envAfterValue, source, "return type")
        expectedErrors
    val tpe = if expectedReturn == Type.Unknown then typedValue.tpe else expectedReturn
    val (updatedEnv, checkedType, expectedErrors) = ensureExpectedType(tpe, Some(expectedType), source, envAfterValue)
    (Expr.Return(typedValue, checkedType)(source), valueErrors ++ typeErrors ++ expectedErrors, updatedEnv)

  private def typeExprs(
      exprs: List[ast.Expr],
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (List[Expr], List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    var currentEnv = env
    val typed = exprs.map { expr =>
      val (typedExpr, exprErrors, updatedEnv) = typeExpr(expr, currentEnv, expectedReturn, expectedType, idSupply)
      errors ++= exprErrors
      currentEnv = updatedEnv
      typedExpr
    }
    (typed, errors.toList, currentEnv)

  private def typePattern(
      pattern: ast.Pattern,
      expectedType: Type,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Pattern, Map[String, TermSymbol], List[TypeError]) =
    val source = pattern.source
    pattern match
      case ast.Pattern.Wildcard() =>
        (Pattern.Wildcard()(pattern.source), Map.empty, Nil)
      case ast.Pattern.Lit(value) =>
        val errors = ListBuffer.empty[TypeError]
        val litType = literalType(value, source, env, errors)
        if !isCompatible(expectedType, litType) then
          errors += errorAt(
            source,
            s"Pattern literal has type ${renderType(litType)}, expected ${renderType(expectedType)}"
          )
        (Pattern.Lit(value)(source), Map.empty, errors.toList)
      case ast.Pattern.BinderOrCtor0(name) =>
        env.exports.ctors.get(name) match
          case Some(ctor) if ctor.arity == 0 =>
            val typeParamBindings =
              if expectedType != Type.Unknown then
                collectTypeParamBindings(ctor.resultType, expectedType, ctor.typeParams.toSet, Map.empty)
              else
                Map.empty[String, Type]
            val instantiatedResultType = instantiateType(ctor.resultType, typeParamBindings)
            val errors =
              if isCompatible(expectedType, instantiatedResultType) then Nil
              else List(
                errorAt(
                  source,
                  s"Constructor $name has type ${renderType(instantiatedResultType)}, expected ${renderType(expectedType)}"
                )
              )
            (Pattern.Ctor(ctor, Nil)(source), Map.empty, errors)
          case _ =>
            val symbol = LocalSymbol(name, expectedType, idSupply.freshId())
            (Pattern.Binder(symbol)(source), Map(name -> symbol), Nil)
      case ast.Pattern.Ctor(name, args) =>
        env.exports.ctors.get(name) match
          case Some(ctor) =>
            val Type.Fun(paramTypes, resultType) = ctor.tpe
            val subst =
              if expectedType != Type.Unknown then
                collectTypeParamBindings(resultType, expectedType, ctor.typeParams.toSet, Map.empty)
              else
                Map.empty[String, Type]
            val instantiatedParamTypes = paramTypes.map(p => instantiateType(p, subst))
            val instantiatedResultType = instantiateType(resultType, subst)
            val errors = ListBuffer.empty[TypeError]
            if ctor.arity != args.length then
              errors += errorAt(source, s"Constructor $name expects ${ctor.arity} args, got ${args.length}")
            if !isCompatible(expectedType, instantiatedResultType) then
              errors += errorAt(
                source,
                s"Constructor $name returns ${renderType(instantiatedResultType)}, expected ${renderType(expectedType)}"
              )
            val typedArgs = args
              .zipAll(instantiatedParamTypes, ast.Pattern.Wildcard()(source), Type.Unknown)
              .map { case (arg, tpe) =>
                val (typedArg, bindings, argErrors) = typePattern(arg, tpe, env, idSupply)
                errors ++= argErrors
                (typedArg, bindings)
              }
            val bindings = mergeBindings(typedArgs.map(_._2), errors, source)
            (Pattern.Ctor(ctor, typedArgs.map(_._1))(source), bindings, errors.toList)
          case None =>
            val unknownCtor = CtorSymbol(name, Nil, Type.Fun(Nil, Type.Unknown), 0, Type.Unknown)
            (
              Pattern.Ctor(unknownCtor, Nil)(source),
              Map.empty,
              List(errorAt(source, s"Unknown constructor: $name"))
            )

  private def mergeBindings(
      bindingSets: List[Map[String, TermSymbol]],
      errors: ListBuffer[TypeError],
      source: ast.SourceRange
    ): Map[String, TermSymbol] =
    bindingSets.foldLeft(Map.empty[String, TermSymbol]) { (acc, bindings) =>
      bindings.foreach { case (name, _) =>
        if acc.contains(name) then
          errors += errorAt(source, s"Duplicate pattern binder: $name")
      }
      acc ++ bindings
    }

  private def collectTypeParamBindings(
      pattern: Type,
      actual: Type,
      typeParams: Set[String],
      bindings: Map[String, Type]
    ): Map[String, Type] =
    (pattern, actual) match
      case (Type.Name(name), _) if typeParams.contains(name) =>
        bindings.get(name) match
          case Some(existing) =>
            if isCompatible(existing, actual) then
              bindings.updated(name, unifyTypesSimple(existing, actual))
            else
              bindings
          case None =>
            bindings.updated(name, actual)
      case (Type.App(patternBase, patternArgs), Type.App(actualBase, actualArgs)) =>
        val baseBindings = collectTypeParamBindings(patternBase, actualBase, typeParams, bindings)
        patternArgs.zipAll(actualArgs, Type.Unknown, Type.Unknown).foldLeft(baseBindings) {
          case (current, (patternArg, actualArg)) =>
            collectTypeParamBindings(patternArg, actualArg, typeParams, current)
        }
      case (Type.Fun(patternParams, patternResult), Type.Fun(actualParams, actualResult)) =>
        val paramBindings =
          patternParams.zipAll(actualParams, Type.Unknown, Type.Unknown).foldLeft(bindings) {
            case (current, (patternParam, actualParam)) =>
              collectTypeParamBindings(patternParam, actualParam, typeParams, current)
          }
        collectTypeParamBindings(patternResult, actualResult, typeParams, paramBindings)
      case _ => bindings

  private def instantiateType(tpe: Type, bindings: Map[String, Type]): Type =
    tpe match
      case Type.Name(name) => bindings.getOrElse(name, tpe)
      case Type.App(base, args) => Type.App(instantiateType(base, bindings), args.map(instantiateType(_, bindings)))
      case Type.Fun(params, result) =>
        Type.Fun(params.map(instantiateType(_, bindings)), instantiateType(result, bindings))
      case Type.Meta(id) => Type.Meta(id)
      case Type.Unknown => Type.Unknown

  private def dropSelfBindings(bindings: Map[String, Type]): Map[String, Type] =
    bindings.filterNot { case (name, tpe) => containsTypeParam(tpe, name) }

  private def containsTypeParam(tpe: Type, name: String): Boolean =
    tpe match
      case Type.Name(value) => value == name
      case Type.App(base, args) => containsTypeParam(base, name) || args.exists(containsTypeParam(_, name))
      case Type.Fun(params, result) =>
        params.exists(containsTypeParam(_, name)) || containsTypeParam(result, name)
      case Type.Meta(_) => false
      case Type.Unknown => false

  private def iterableElementType(tpe: Type, source: ast.SourceRange): (Type, List[TypeError]) =
    tpe match
      case Type.App(Type.Name("List"), List(elem)) => (elem, Nil)
      case Type.App(Type.Name("Set"), List(elem)) => (elem, Nil)
      case Type.App(Type.Name("Map"), List(key, _)) => (key, Nil)
      case Type.Unknown => (Type.Unknown, Nil)
      case _ => (Type.Unknown, List(errorAt(source, s"Expected iterable, got ${renderType(tpe)}")))

  private def suiteType(suite: Suite): Type =
    suite match
      case Suite.Single(expr) => expr.tpe
      case Suite.Block(_, tpe) => tpe

  private def unifyBranchTypes(
      context: String,
      left: Type,
      right: Type,
      source: ast.SourceRange,
      env: TypeEnv
    ): (TypeEnv, Type, List[TypeError]) =
    unifyTypes(left, right, env, source, s"$context branches")

  private def ensureExpectedType(
      actualType: Type,
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv
    ): (TypeEnv, Type, List[TypeError]) =
    expectedType match
      case Some(expected) =>
        val (updatedEnv, unifiedType, errors) =
          unifyTypes(expected, actualType, env, source, "expected type")
        (updatedEnv, unifiedType, errors)
      case None => (env, actualType, Nil)

  // Applies the current type substitutions to a type.
  private def applySubstitutions(tpe: Type, env: TypeEnv): Type =
    tpe match
      case Type.Meta(id) =>
        env.substitutions.get(id) match
          case Some(bound) => applySubstitutions(bound, env)
          case None => tpe
      case Type.App(base, args) =>
        Type.App(applySubstitutions(base, env), args.map(applySubstitutions(_, env)))
      case Type.Fun(params, result) =>
        Type.Fun(params.map(applySubstitutions(_, env)), applySubstitutions(result, env))
      case other => other

  // Checks whether a meta variable occurs in a type.
  private def occursInMeta(tpe: Type, id: Int, env: TypeEnv): Boolean =
    applySubstitutions(tpe, env) match
      case Type.Meta(otherId) => id == otherId
      case Type.App(base, args) => occursInMeta(base, id, env) || args.exists(occursInMeta(_, id, env))
      case Type.Fun(params, result) =>
        params.exists(occursInMeta(_, id, env)) || occursInMeta(result, id, env)
      case _ => false

  // Replaces a meta variable in a type with a concrete type.
  

  // Unifies two types and updates the type environment substitutions.
  private def unifyTypes(
      left: Type,
      right: Type,
      env: TypeEnv,
      source: ast.SourceRange,
      context: String
    ): (TypeEnv, Type, List[TypeError]) =
    val resolvedLeft = applySubstitutions(left, env)
    val resolvedRight = applySubstitutions(right, env)
    (resolvedLeft, resolvedRight) match
      case (Type.Unknown, other) => (env, other, Nil)
      case (other, Type.Unknown) => (env, other, Nil)
      case (Type.Meta(id), other) =>
        if other == Type.Meta(id) then (env, other, Nil)
        else if occursInMeta(other, id, env) then
          (env, Type.Unknown, List(errorAt(source, s"Cannot construct infinite type in $context")))
        else
          val updatedSubst = env.substitutions.updated(id, other)
          (env.copy(substitutions = updatedSubst), other, Nil)
      case (other, Type.Meta(id)) =>
        unifyTypes(Type.Meta(id), other, env, source, context)
      case (Type.Name(leftName), Type.Name(rightName)) if leftName == rightName =>
        (env, resolvedLeft, Nil)
      case (Type.App(leftBase, leftArgs), Type.App(rightBase, rightArgs)) =>
        val (envAfterBase, unifiedBase, baseErrors) =
          unifyTypes(leftBase, rightBase, env, source, context)
        val initial = (envAfterBase, List.empty[Type], baseErrors)
        val (envAfterArgs, unifiedArgs, argErrors) =
          leftArgs
            .zipAll(rightArgs, Type.Unknown, Type.Unknown)
            .foldLeft(initial) { case ((currentEnv, acc, errors), (lArg, rArg)) =>
              val (nextEnv, unifiedArg, newErrors) = unifyTypes(lArg, rArg, currentEnv, source, context)
              (nextEnv, acc :+ unifiedArg, errors ++ newErrors)
            }
        (
          envAfterArgs,
          Type.App(unifiedBase, unifiedArgs),
          argErrors
        )
      case (Type.Fun(leftParams, leftResult), Type.Fun(rightParams, rightResult)) =>
        val initial = (env, List.empty[Type], List.empty[TypeError])
        val (envAfterParams, unifiedParams, paramErrors) =
          leftParams
            .zipAll(rightParams, Type.Unknown, Type.Unknown)
            .foldLeft(initial) { case ((currentEnv, acc, errors), (lParam, rParam)) =>
              val (nextEnv, unifiedParam, newErrors) = unifyTypes(lParam, rParam, currentEnv, source, context)
              (nextEnv, acc :+ unifiedParam, errors ++ newErrors)
            }
        val (envAfterResult, unifiedResult, resultErrors) =
          unifyTypes(leftResult, rightResult, envAfterParams, source, context)
        (
          envAfterResult,
          Type.Fun(unifiedParams, unifiedResult),
          paramErrors ++ resultErrors
        )
      case _ =>
        (
          env,
          Type.Unknown,
          List(errorAt(source, s"Type mismatch in $context: ${renderType(resolvedLeft)} vs ${renderType(resolvedRight)}"))
        )

  private def resolveFunctionType(callee: Expr, env: TypeEnv): Option[(Type.Fun, List[String], List[Type])] =
    callee match
      case Expr.Var(symbol, _) =>
        symbol match
          case fun: FunctionSymbol => Some((fun.tpe, fun.typeParams, fun.givenParams))
          case _ => None
      case _ =>
        applySubstitutions(callee.tpe, env) match
          case tpe: Type.Fun => Some((tpe, Nil, Nil))
          case _ => None

  private def isCompatible(expected: Type, actual: Type): Boolean =
    (expected, actual) match
      case (Type.Unknown, _) => true
      case (_, Type.Unknown) => true
      case (Type.Meta(_), _) => true
      case (_, Type.Meta(_)) => true
      case (Type.Name(left), Type.Name(right)) => left == right
      case (Type.App(leftBase, leftArgs), Type.App(rightBase, rightArgs)) =>
        isCompatible(leftBase, rightBase) &&
          leftArgs
            .zipAll(rightArgs, Type.Unknown, Type.Unknown)
            .forall { case (left, right) => isCompatible(left, right) }
      case (Type.Fun(leftParams, leftResult), Type.Fun(rightParams, rightResult)) =>
        leftParams
          .zipAll(rightParams, Type.Unknown, Type.Unknown)
          .forall { case (left, right) => isCompatible(left, right) } &&
          isCompatible(leftResult, rightResult)
      case _ => false

  // Performs a simple unification without meta substitutions.
  private def unifyTypesSimple(left: Type, right: Type): Type =
    (left, right) match
      case (Type.Unknown, other) => other
      case (other, Type.Unknown) => other
      case (Type.Meta(_), other) => other
      case (other, Type.Meta(_)) => other
      case _ if left == right => left
      case _ => Type.Unknown

  private def resolveSymbol(name: String, env: TypeEnv): Option[Symbol] =
    env.resolveLocal(name)
      .orElse(baseValues.get(name))
      .orElse(env.exports.instances.get(name).map(_.symbol))
      .orElse(env.exports.functions.get(name))
      .orElse(env.exports.ctors.get(name))

  // Resolves a type name from the standard library or builtins.
  private def resolveTypeName(
      name: String,
      source: ast.SourceRange,
      env: TypeEnv,
      errors: ListBuffer[TypeError]
    ): Type =
    baseTypes.get(name)
      .orElse(env.exports.types.get(name).map(_ => Type.Name(name)))
      .orElse {
        if builtinTypeNames.contains(name) then Some(Type.Name(name)) else None
      }
      .getOrElse {
        errors += errorAt(source, s"Unknown type name: $name")
        Type.Unknown
      }

  private def fromAstType(tpe: ast.Type): Type =
    tpe match
      case ast.Type.Name(value) => Type.Name(value)
      case ast.Type.Paren(inner) => fromAstType(inner)
      case ast.Type.App(base, args) => Type.App(fromAstType(base), args.map(fromAstType))

  private def validateAstType(tpe: ast.Type, typeParams: Set[String], exports: ExportEnv): List[TypeError] =
    tpe match
      case ast.Type.Name(value) =>
        if typeParams.contains(value) || builtinTypeNames.contains(value) || exports.types.contains(value) || exports.typeClasses.contains(value) then Nil
        else List(errorAt(tpe.source, s"Unknown type name: $value"))
      case ast.Type.App(base, args) =>
        validateAstType(base, typeParams, exports) ++ args.flatMap(arg => validateAstType(arg, typeParams, exports))
      case ast.Type.Paren(inner) =>
        validateAstType(inner, typeParams, exports)

  private def validateTypedType(
      tpe: Type,
      typeParams: Set[String],
      exports: ExportEnv,
      source: ast.SourceRange
    ): List[TypeError] =
    tpe match
      case Type.Name(value) =>
        if typeParams.contains(value) || builtinTypeNames.contains(value) || exports.types.contains(value) || exports.typeClasses.contains(value) then Nil
        else List(errorAt(source, s"Unknown type name: $value"))
      case Type.App(base, args) =>
        validateTypedType(base, typeParams, exports, source) ++ args.flatMap(arg => validateTypedType(arg, typeParams, exports, source))
      case Type.Fun(params, result) =>
        params.flatMap(param => validateTypedType(param, typeParams, exports, source)) ++ validateTypedType(result, typeParams, exports, source)
      case Type.Meta(_) => Nil
      case Type.Unknown => Nil

  // Resolves literal types based on the standard library.
  private def literalType(
      literal: ast.Literal,
      source: ast.SourceRange,
      env: TypeEnv,
      errors: ListBuffer[TypeError]
    ): Type =
    literal match
      case ast.Literal.IntLit(_) => resolveTypeName("Int", source, env, errors)
      case ast.Literal.BoolLit(_) => resolveTypeName("Bool", source, env, errors)
      case ast.Literal.StringLit(_) => resolveTypeName("String", source, env, errors)
      case ast.Literal.UnitLit() => resolveTypeName("unit", source, env, errors)

  private def renderType(tpe: Type): String =
    tpe match
      case Type.Name(value) => value
      case Type.App(base, args) => s"${renderType(base)}[${args.map(renderType).mkString(", ")}]"
      case Type.Fun(params, result) =>
        s"(${params.map(renderType).mkString(", ")}) -> ${renderType(result)}"
      case Type.Meta(id) => s"?$id"
      case Type.Unknown => "Unknown"

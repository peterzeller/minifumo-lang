package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.typing.TypedAst.*

import scala.collection.mutable.ListBuffer
import com.github.peterzeller.minifumo.common.MinifumoError

object TypeChecker:
  final case class TypeError(message: String, source: ast.SourceRange) extends MinifumoError

  final case class DataType(name: String, typeParams: List[String], ctors: List[CtorSymbol])

  final case class ExportEnv(
      functions: Map[String, FunctionSymbol],
      ctors: Map[String, CtorSymbol],
      types: Map[String, DataType]
    )

  val emptyExportEnv: ExportEnv =
    ExportEnv(Map.empty, Map.empty, Map.empty)

  final case class TypeEnv(
      scopes: List[Map[String, TermSymbol]],
      exports: ExportEnv,
      typeParams: Set[String],
      expectedReturn: Type,
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
    ExportEnv(
      functions = base.functions ++ overrideEnv.functions,
      ctors = base.ctors ++ overrideEnv.ctors,
      types = base.types ++ overrideEnv.types
    )

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
    val (exports, exportErrors) =
      extractExports(
        program,
        combinedExports,
        includeNonExported = true,
        shadowedTypes = shadowedTypes,
        shadowedCtors = shadowedCtors
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
    }
    (Program(typedItems)(program.source), errors.toList)

  // Extracts exported symbols from a program, optionally merging base exports for imports.
  def extractExports(
      program: ast.ProgramFile,
      baseExports: ExportEnv = emptyExportEnv,
      includeNonExported: Boolean = true,
      shadowedTypes: Set[String] = Set.empty,
      shadowedCtors: Set[String] = Set.empty
    ): (ExportEnv, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    var functions = baseExports.functions
    var ctors = baseExports.ctors
    var types = baseExports.types
    val localTypes = scala.collection.mutable.Set.empty[String]
    val localCtors = scala.collection.mutable.Set.empty[String]

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
            errors ++= validateAstType(field.tpe, typeParams.toSet, ExportEnv(functions, ctors, types))
          }
        }
        val eliminatorName = buildEliminatorName(name)
        val allowEliminatorOverride = shadowedTypes.contains(name)
        if functions.contains(eliminatorName) && !allowEliminatorOverride then
          errors += errorAt(item.source, s"Duplicate function: $eliminatorName")
        else
          val returnTypeParam = "R"
          val returnType = Type.Name(returnTypeParam)
          val scrutineeType = resultType
          val handlerTypes = ctorSymbols.map { ctor =>
            val handlerParams =
              if ctor.tpe.params.isEmpty then List(baseTypes("unit")) else ctor.tpe.params
            Type.Fun(handlerParams, returnType)
          }
          val eliminatorTypeParams = typeParams :+ returnTypeParam
          val eliminatorParams = scrutineeType +: handlerTypes
          val eliminatorType: Type.Fun = Type.Fun(eliminatorParams, returnType)
          val eliminatorSymbol =
            FunctionSymbol(eliminatorName, eliminatorTypeParams, eliminatorType)
          functions = functions + (eliminatorName -> eliminatorSymbol)
      case funDecl @ ast.TopLevel.FunDecl(name, typeParams, params, returnType, _, exported) if includeNonExported || exported =>
        if functions.contains(name) then
          errors += errorAt(item.source, s"Duplicate function: $name")
        val paramTypes = params.map(p => fromAstType(p.tpe))
        val returnTpe = returnType.map(fromAstType).getOrElse {
          errors += errorAt(item.source, s"Missing return type for function: $name")
          Type.Unknown
        }
        val funSymbol = FunctionSymbol(name, typeParams, Type.Fun(paramTypes, returnTpe))
        functions = functions + (name -> funSymbol)
        val typeParamSet = typeParams.toSet
        params.foreach { param =>
          errors ++= validateAstType(param.tpe, typeParamSet, ExportEnv(functions, ctors, types))
        }
        returnType.foreach { tpe =>
          errors ++= validateAstType(tpe, typeParamSet, ExportEnv(functions, ctors, types))
        }
      case _ =>
    }
    (ExportEnv(functions, ctors, types), errors.toList)

  def typeFunction(
      funDecl: ast.TopLevel.FunDecl,
      exports: ExportEnv
    ): (TopLevel.FunDecl, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val idSupply = new IdSupply
    val funSymbol = exports.functions.getOrElse(
      funDecl.name,
      FunctionSymbol(funDecl.name, funDecl.typeParams, Type.Fun(Nil, Type.Unknown))
    )
    val paramsWithSource = funDecl.params.map { param =>
      (param, ParamSymbol(param.name, fromAstType(param.tpe), idSupply.freshId()))
    }
    val duplicateParams = funDecl.params.groupBy(_.name).collect { case (_, ps) if ps.length > 1 => ps }
    duplicateParams.flatten.foreach { param =>
      errors += errorAt(param.source, s"Duplicate parameter: ${param.name}")
    }
    val params = paramsWithSource.map(_._2)
    val scopeBindings = params.map(p => p.name -> p).toMap
    val env =
      TypeEnv(List(scopeBindings), exports, funDecl.typeParams.toSet, funSymbol.tpe.result, Map.empty, 1)
    val expectedBodyType =
      if funSymbol.tpe.result == Type.Unknown then None else Some(funSymbol.tpe.result)
    val (typedBody, bodyErrors, _) = typeExpr(funDecl.body, env, funSymbol.tpe.result, expectedBodyType, idSupply)
    errors ++= bodyErrors
    val typedFun: TopLevel.FunDecl =
      TopLevel.FunDecl(funSymbol, funDecl.typeParams, params, typedBody)(funDecl.source)
    (typedFun, errors.toList)

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
      case ast.Expr.Call(callee, typeArgs, args) =>
        synthCall(callee, typeArgs, args, expr.source, env, idSupply)
      case ast.Expr.Lambda(param, body) =>
        synthLambda(param, body, expr.source, env, idSupply)
      case ast.Expr.LetIn(name, isConstant, declaredTypeAst, valueExpr, bodyExpr) =>
        synthLetIn(name, isConstant, declaredTypeAst, valueExpr, bodyExpr, expr.source, env, idSupply)
      case ast.Expr.Bind(name, isConstant, declaredTypeAst, valueExpr) =>
        synthBind(name, isConstant, declaredTypeAst, valueExpr, expr.source, env, idSupply)
      case ast.Expr.Match(scrutinee, cases) =>
        synthMatchElim(scrutinee, cases, expr.source, env, idSupply)
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
      case ast.Expr.Call(callee, typeArgs, args) =>
        checkCall(callee, typeArgs, args, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Lambda(param, body) =>
        checkLambda(param, body, expectedType, expr.source, env, expectedReturn, idSupply)
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
      case ast.Expr.Match(scrutinee, cases) =>
        checkMatchElim(scrutinee, cases, expectedType, expr.source, env, idSupply)
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

  // T-App: Γ ⊢ f ⇒ T1 → T2  and  Γ ⊢ a ⇐ T1  ⇒  Γ ⊢ f a ⇒ T2
  private def synthCall(
      callee: ast.Expr,
      typeArgs: List[ast.Type],
      args: List[ast.Expr],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    checkCall(callee, typeArgs, args, Type.Unknown, source, env, env.expectedReturn, idSupply)

  // T-App-Check: Γ ⊢ f ⇒ T1 → T2  and  Γ ⊢ a ⇐ T1  and  T2 ≈ T  ⇒  Γ ⊢ f a ⇐ T
  private def checkCall(
      callee: ast.Expr,
      typeArgs: List[ast.Type],
      args: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedCallee, calleeErrors, envAfterCallee) = synthesizeExpr(callee, env, idSupply)
    typedCallee match
      case Expr.Var(symbol, _) if symbol.name == "." && args.length == 2 =>
        val (typedArgs, argsErrors, envAfterArgs) =
          typeExprs(args, envAfterCallee, expectedReturn, None, idSupply)
        val errors = calleeErrors ++ argsErrors :+ errorAt(callee.source, "Field access typing is not implemented")
        (Expr.CallFun(typedCallee, typedArgs, Type.Unknown)(source), errors, envAfterArgs)
      case Expr.Var(symbol: CtorSymbol, _) =>
        val (typedExpr, ctorErrors, envAfterCtor) =
          typeCtorCall(symbol, args, expectedType, source, envAfterCallee, expectedReturn, idSupply)
        (typedExpr, calleeErrors ++ ctorErrors, envAfterCtor)
      case _ =>
        val maybeFunType = resolveFunctionType(typedCallee, envAfterCallee)
        maybeFunType match
          case Some((funType, typeParams)) =>
            val errors = ListBuffer.empty[TypeError]
            errors ++= calleeErrors
            val (envAfterTypeParams, typeParamBindings, typeArgErrors) =
              instantiateTypeParams(typeParams, typeArgs, envAfterCallee, source)
            errors ++= typeArgErrors
            val instantiatedFunType: Type.Fun = instantiateType(funType, typeParamBindings) match
              case fun: Type.Fun => fun
              case _ => Type.Fun(Nil, Type.Unknown)
            var currentEnv = envAfterTypeParams
            var remainingParams = instantiatedFunType.params
            val typedArgs = args.map { arg =>
              val expectedParamType =
                remainingParams.headOption
                  .map(applySubstitutions(_, currentEnv))
                  .getOrElse(Type.Unknown)
              val (typedArg, argErrors, updatedEnv) =
                checkExpr(arg, currentEnv, expectedReturn, expectedParamType, idSupply)
              errors ++= argErrors
              currentEnv = updatedEnv
              remainingParams = remainingParams.drop(1)
              typedArg
            }
            if args.length > instantiatedFunType.params.length then
              errors += errorAt(
                callee.source,
                s"Call expects at most ${instantiatedFunType.params.length} args, got ${args.length}"
              )
            val resultAfterArgs =
              if remainingParams.nonEmpty then
                Type.Fun(remainingParams, instantiatedFunType.result)
              else instantiatedFunType.result
            val resultWithSubstitutions = applySubstitutions(resultAfterArgs, currentEnv)
            val (envAfterExpected, checkedType, expectedErrors) =
              ensureExpectedType(resultWithSubstitutions, Some(expectedType), source, currentEnv)
            errors ++= expectedErrors
            (
              Expr.CallFun(typedCallee, typedArgs, checkedType)(source),
              errors.toList,
              envAfterExpected
            )
          case None =>
            val (typedArgs, argsErrors, envAfterArgs) = typeExprs(args, envAfterCallee, expectedReturn, None, idSupply)
            val errors =
              calleeErrors ++ argsErrors :+ errorAt(
                callee.source,
                s"Call target is not a function: ${renderType(typedCallee.tpe)}"
              )
            (Expr.CallFun(typedCallee, typedArgs, Type.Unknown)(source), errors, envAfterArgs)


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
    var currentEnv = envAfterTypeParams
    var remainingParams = instantiatedCtorType.params
    val typedArgs = args.map { arg =>
      val expectedParamType =
        remainingParams.headOption
          .map(applySubstitutions(_, currentEnv))
          .getOrElse(Type.Unknown)
      val (typedArg, argErrors, updatedEnv) =
        checkExpr(arg, currentEnv, expectedReturn, expectedParamType, idSupply)
      errors ++= argErrors
      currentEnv = updatedEnv
      remainingParams = remainingParams.drop(1)
      typedArg
    }
    if args.length > instantiatedCtorType.params.length then
      errors += errorAt(
        source,
        s"Constructor ${ctor.name} expects at most ${instantiatedCtorType.params.length} args, got ${args.length}"
      )
    val resultAfterArgs =
      if remainingParams.nonEmpty then
        Type.Fun(remainingParams, instantiatedCtorType.result)
      else instantiatedCtorType.result
    val (envAfterExpected, checkedType, expectedErrors) =
      ensureExpectedType(resultAfterArgs, Some(expectedType), source, currentEnv)
    errors ++= expectedErrors
    (Expr.CallCtor(ctor, typedArgs, checkedType)(source), errors.toList, envAfterExpected)

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

  // T-Lam: Γ,x:T1 ⊢ e ⇒ T2  ⇒  Γ ⊢ (x -> e) ⇒ T1 → T2
  private def synthLambda(
      param: ast.LambdaParam,
      body: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val declaredType = param.tpe.map(fromAstType)
    errors ++= param.tpe.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val (envAfterParam, paramType) = declaredType match
      case Some(tpe) => (env, tpe)
      case None => env.freshMeta()
    val symbol = LocalSymbol(param.name, paramType, idSupply.freshId())
    val (typedBody, bodyErrors, envAfterBody) =
      synthesizeExpr(body, envAfterParam.withBinding(symbol), idSupply)
    errors ++= bodyErrors
    val lamType = Type.Fun(List(paramType), typedBody.tpe)
    (Expr.Lambda(symbol, typedBody, lamType)(source), errors.toList, envAfterBody)

  // T-Lam-Check: Γ ⊢ (x -> e) ⇐ T1 → T2  if  Γ,x:T1 ⊢ e ⇐ T2
  private def checkLambda(
      param: ast.LambdaParam,
      body: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    expectedType match
      case Type.Fun(params, result) if params.nonEmpty =>
        val errors = ListBuffer.empty[TypeError]
        val declaredType = param.tpe.map(fromAstType)
        errors ++= param.tpe.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
        val expectedParamType = params.head
        val (envAfterParam, paramType, paramErrors) =
          declaredType match
            case Some(tpe) => unifyTypes(tpe, expectedParamType, env, source, "lambda param")
            case None => (env, expectedParamType, Nil)
        errors ++= paramErrors
        val expectedBodyType = if params.length > 1 then Type.Fun(params.tail, result) else result
        val symbol = LocalSymbol(param.name, paramType, idSupply.freshId())
        val (typedBody, bodyErrors, envAfterBody) =
          checkExpr(body, envAfterParam.withBinding(symbol), expectedReturn, expectedBodyType, idSupply)
        errors ++= bodyErrors
        val lamType = Type.Fun(List(paramType), typedBody.tpe)
        val (envAfterExpected, checkedType, expectedErrors) =
          ensureExpectedType(lamType, Some(expectedType), source, envAfterBody)
        errors ++= expectedErrors
        (
          Expr.Lambda(symbol, typedBody, checkedType)(source),
          errors.toList,
          envAfterExpected
        )
      case _ =>
        val (typed, errors, envAfter) = synthLambda(param, body, source, env, idSupply)
        val (envAfterExpected, checkedType, expectedErrors) =
          ensureExpectedType(typed.tpe, Some(expectedType), source, envAfter)
        val updatedExpr = typed match
          case Expr.Lambda(symbol, typedBody, _) => Expr.Lambda(symbol, typedBody, checkedType)(source)
          case other => other
        (updatedExpr, errors ++ expectedErrors, envAfterExpected)

  // Desugars match expressions into eliminator calls.
  private def synthMatchElim(
      scrutinee: ast.Expr,
      cases: List[ast.MatchCase],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedScrutinee, scrutineeErrors, envAfterScrutinee) = synthesizeExpr(scrutinee, env, idSupply)
    val errors = ListBuffer.empty[TypeError]
    errors ++= scrutineeErrors
    val (typedCases, caseErrors, envAfterCases) =
      typeMatchCases(cases, typedScrutinee.tpe, None, source, envAfterScrutinee, idSupply)
    errors ++= caseErrors
    val caseTypes = typedCases.map(c => c.body.tpe)
    val (envAfterResult, resultType, resultErrors) =
      caseTypes.drop(1).foldLeft((envAfterCases, caseTypes.headOption.getOrElse(Type.Unknown), List.empty[TypeError])) {
        case ((envAcc, accType, accErrors), nextType) =>
          val (nextEnv, unified, nextErrors) =
            unifyTypes(accType, nextType, envAcc, source, "match cases")
          (nextEnv, unified, accErrors ++ nextErrors)
      }
    errors ++= resultErrors
    val (handlerArgs, handlerErrors) =
      buildMatchHandlers(typedScrutinee, typedCases, source, envAfterResult, idSupply)
    errors ++= handlerErrors
    val (elimExpr, elimErrors, envAfterElim) =
      buildEliminatorCall(typedScrutinee, handlerArgs, resultType, source, envAfterResult)
    errors ++= elimErrors
    (elimExpr, errors.toList, envAfterElim)

  // Desugars match expressions into eliminator calls with an expected result type.
  private def checkMatchElim(
      scrutinee: ast.Expr,
      cases: List[ast.MatchCase],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError], TypeEnv) =
    val (typedScrutinee, scrutineeErrors, envAfterScrutinee) = synthesizeExpr(scrutinee, env, idSupply)
    val errors = ListBuffer.empty[TypeError]
    errors ++= scrutineeErrors
    val (typedCases, caseErrors, envAfterCases) =
      typeMatchCases(cases, typedScrutinee.tpe, Some(expectedType), source, envAfterScrutinee, idSupply)
    errors ++= caseErrors
    val caseTypes = typedCases.map(c => c.body.tpe)
    val mismatchErrors = caseTypes.sliding(2).collect {
      case List(a, b) if !isCompatible(a, b) =>
        errorAt(source, s"Match case types do not agree: ${renderType(a)} vs ${renderType(b)}")
    }.toList
    errors ++= mismatchErrors
    val (handlerArgs, handlerErrors) =
      buildMatchHandlers(typedScrutinee, typedCases, source, envAfterCases, idSupply)
    errors ++= handlerErrors
    val (elimExpr, elimErrors, envAfterElim) =
      buildEliminatorCall(typedScrutinee, handlerArgs, expectedType, source, envAfterCases)
    errors ++= elimErrors
    (elimExpr, errors.toList, envAfterElim)

  private final case class TypedMatchCase(pattern: Pattern, body: Expr, source: ast.SourceRange)

  // Types match cases against the scrutinee type and optional expected type.
  private def typeMatchCases(
      cases: List[ast.MatchCase],
      scrutineeType: Type,
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (List[TypedMatchCase], List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    var currentEnv = env
    val typedCases = cases.map { matchCase =>
      val (typedPattern, bindings, patternErrors) =
        typePattern(matchCase.pattern, scrutineeType, currentEnv, idSupply)
      errors ++= patternErrors
      val envWithBindings = bindings.values.foldLeft(currentEnv) { case (current, symbol) => current.withBinding(symbol) }
      val (typedBody, bodyErrors, envAfterBody) =
        typeExpr(matchCase.body, envWithBindings, env.expectedReturn, expectedType, idSupply)
      errors ++= bodyErrors
      currentEnv = envAfterBody
      TypedMatchCase(typedPattern, typedBody, matchCase.source)
    }
    if typedCases.isEmpty then
      errors += errorAt(source, "Match expression requires at least one case")
    (typedCases, errors.toList, currentEnv)

  // Builds handler expressions for a match by mapping cases to constructors.
  private def buildMatchHandlers(
      scrutinee: Expr,
      cases: List[TypedMatchCase],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (List[Expr], List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val maybeDataType = resolveDataType(scrutinee.tpe, source, env, errors)
    val handlers = maybeDataType.toList.flatMap { case (dataType, bindings) =>
      dataType.ctors.map { ctor =>
        val matchingCase = cases.find(caseMatchesCtor(_, ctor))
        matchingCase match
          case Some(caseInfo) =>
            buildHandlerForCase(scrutinee.tpe, ctor, bindings, caseInfo, source, idSupply, errors)
          case None =>
            errors += errorAt(source, s"No match case for constructor ${ctor.name}")
            buildFallbackHandler(ctor, bindings, source, idSupply)
      }
    }
    (handlers, errors.toList)

  // Builds the eliminator call expression from the scrutinee and handlers.
  private def buildEliminatorCall(
      scrutinee: Expr,
      handlers: List[Expr],
      resultType: Type,
      source: ast.SourceRange,
      env: TypeEnv
    ): (Expr, List[TypeError], TypeEnv) =
    val errors = ListBuffer.empty[TypeError]
    val maybeDataType = resolveDataType(scrutinee.tpe, source, env, errors)
    val elimExpr =
      maybeDataType match
        case Some((dataType, _)) =>
          val elimName = buildEliminatorName(dataType.name)
          env.exports.functions.get(elimName) match
            case Some(elimSymbol) =>
              val elimVar = Expr.Var(elimSymbol, elimSymbol.tpe)(source)
              Expr.CallFun(elimVar, scrutinee :: handlers, resultType)(source)
            case None =>
              errors += errorAt(source, s"Unknown eliminator function: $elimName")
              Expr.Lit(ast.Literal.UnitLit()(source), baseTypes("unit"))(source)
        case None =>
          Expr.Lit(ast.Literal.UnitLit()(source), baseTypes("unit"))(source)
    (elimExpr, errors.toList, env)

  // Resolves a data type and its type parameter bindings from a scrutinee type.
  private def resolveDataType(
      scrutineeType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      errors: ListBuffer[TypeError]
    ): Option[(DataType, Map[String, Type])] =
    val dataTypeName = scrutineeType match
      case Type.Name(name) => Some(name)
      case Type.App(Type.Name(name), _) => Some(name)
      case _ =>
        errors += errorAt(source, s"Expected data type, got ${renderType(scrutineeType)}")
        None
    dataTypeName.flatMap { name =>
      env.exports.types.get(name) match
        case Some(dataType) =>
          val template =
            if dataType.typeParams.isEmpty then
              Type.Name(dataType.name)
            else
              Type.App(Type.Name(dataType.name), dataType.typeParams.map(Type.Name.apply))
          val bindings = collectTypeParamBindings(template, scrutineeType, dataType.typeParams.toSet, Map.empty)
          Some((dataType, bindings))
        case None =>
          errors += errorAt(source, s"Expected data type, got ${renderType(scrutineeType)}")
          None
    }

  // Checks whether a typed case pattern can match a constructor.
  private def caseMatchesCtor(caseInfo: TypedMatchCase, ctor: CtorSymbol): Boolean =
    caseInfo.pattern match
      case Pattern.Ctor(symbol, _) => symbol.name == ctor.name
      case Pattern.Wildcard() => true
      case Pattern.Binder(_) => true
      case Pattern.Lit(lit) => literalMatchesCtor(lit, ctor)

  // Builds a handler expression for a specific constructor case.
  private def buildHandlerForCase(
      scrutineeType: Type,
      ctor: CtorSymbol,
      bindings: Map[String, Type],
      caseInfo: TypedMatchCase,
      source: ast.SourceRange,
      idSupply: IdSupply,
      errors: ListBuffer[TypeError]
    ): Expr =
    val instantiatedCtorType: Type.Fun = instantiateType(ctor.tpe, bindings) match
      case fun: Type.Fun => fun
      case _ => Type.Fun(Nil, Type.Unknown)
    val fieldTypes = instantiatedCtorType.params
    val baseBody = caseInfo.body
    caseInfo.pattern match
      case Pattern.Ctor(_, args) =>
        val params = handlerParamsFromPattern(args, fieldTypes, source, idSupply, errors)
        val handlerBody = baseBody
        buildLambdaChain(params, handlerBody, source)
      case Pattern.Wildcard() =>
        val params = wildcardParams(fieldTypes, idSupply)
        buildLambdaChain(params, baseBody, source)
      case Pattern.Binder(symbol) =>
        val params = wildcardParams(fieldTypes, idSupply)
        val ctorArgs = params.map(param => Expr.Var(param, param.tpe)(source))
        val ctorExpr = Expr.CallCtor(ctor, ctorArgs, scrutineeType)(source)
        val letExpr = Expr.LetIn(symbol, true, Some(scrutineeType), ctorExpr, baseBody, baseBody.tpe)(source)
        buildLambdaChain(params, letExpr, source)
      case Pattern.Lit(_) =>
        val params = wildcardParams(fieldTypes, idSupply)
        buildLambdaChain(params, baseBody, source)

  // Builds handler parameters for constructor patterns.
  private def handlerParamsFromPattern(
      args: List[Pattern],
      fieldTypes: List[Type],
      source: ast.SourceRange,
      idSupply: IdSupply,
      errors: ListBuffer[TypeError]
    ): List[LocalSymbol] =
    val normalizedFieldTypes =
      if fieldTypes.isEmpty then List(baseTypes("unit")) else fieldTypes
    val normalizedArgs =
      if fieldTypes.isEmpty then List(Pattern.Wildcard()(source)) else args
    if normalizedArgs.length != normalizedFieldTypes.length then
      errors += errorAt(source, s"Constructor expects ${normalizedFieldTypes.length} args, got ${normalizedArgs.length}")
    normalizedArgs
      .zipAll(normalizedFieldTypes, Pattern.Wildcard()(source), Type.Unknown)
      .map {
        case (Pattern.Binder(symbol), _) => symbol
        case (Pattern.Wildcard(), tpe) => LocalSymbol(s"_arg${idSupply.freshId()}", tpe, idSupply.freshId())
        case (Pattern.Ctor(_, _), tpe) =>
          errors += errorAt(source, "Nested constructor patterns are not supported in eliminators")
          LocalSymbol(s"_arg${idSupply.freshId()}", tpe, idSupply.freshId())
        case (Pattern.Lit(_), tpe) =>
          errors += errorAt(source, "Literal patterns are not supported in eliminators")
          LocalSymbol(s"_arg${idSupply.freshId()}", tpe, idSupply.freshId())
      }

  // Builds wildcard parameters for constructor handlers.
  private def wildcardParams(
      fieldTypes: List[Type],
      idSupply: IdSupply
    ): List[LocalSymbol] =
    val normalizedFieldTypes =
      if fieldTypes.isEmpty then List(baseTypes("unit")) else fieldTypes
    normalizedFieldTypes.map(tpe => LocalSymbol(s"_arg${idSupply.freshId()}", tpe, idSupply.freshId()))

  // Builds nested lambda expressions from handler parameters.
  private def buildLambdaChain(params: List[LocalSymbol], body: Expr, source: ast.SourceRange): Expr =
    params.reverse.foldLeft(body) { (current, param) =>
      Expr.Lambda(param, current, Type.Fun(List(param.tpe), current.tpe))(source)
    }

  // Builds a fallback handler when a match case is missing.
  private def buildFallbackHandler(
      ctor: CtorSymbol,
      bindings: Map[String, Type],
      source: ast.SourceRange,
      idSupply: IdSupply
    ): Expr =
    val instantiatedCtorType: Type.Fun = instantiateType(ctor.tpe, bindings) match
      case fun: Type.Fun => fun
      case _ => Type.Fun(Nil, Type.Unknown)
    val params = wildcardParams(instantiatedCtorType.params, idSupply)
    val body = Expr.Lit(ast.Literal.UnitLit()(source), baseTypes("unit"))(source)
    buildLambdaChain(params, body, source)

  // Checks if a literal pattern can match a constructor.
  private def literalMatchesCtor(literal: ast.Literal, ctor: CtorSymbol): Boolean =
    literal match
      case ast.Literal.BoolLit(value) =>
        val ctorName = if value then "True" else "False"
        ctor.name == ctorName
      case _ => false

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
        val normalizedActual = applySubstitutions(actualType, updatedEnv)
        val normalizedExpected = applySubstitutions(expected, updatedEnv)
        val refinedErrors =
          if errors.nonEmpty && !isCompatible(normalizedExpected, normalizedActual) then
            List(
              errorAt(
                source,
                s"Expression has type ${renderType(normalizedActual)}, expected ${renderType(normalizedExpected)}"
              )
            )
          else
            errors
        (updatedEnv, unifiedType, refinedErrors)
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

  private def resolveFunctionType(callee: Expr, env: TypeEnv): Option[(Type.Fun, List[String])] =
    callee match
      case Expr.Var(symbol, _) =>
        symbol match
          case fun: FunctionSymbol => Some((fun.tpe, fun.typeParams))
          case _ => None
      case _ =>
        applySubstitutions(callee.tpe, env) match
          case tpe: Type.Fun => Some((tpe, Nil))
          case _ => None

  // Builds the implicit eliminator name for a data type.
  private def buildEliminatorName(typeName: String): String =
    s"${typeName}_elim"

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
      case ast.Type.Fun(param, result) => Type.Fun(List(fromAstType(param)), fromAstType(result))

  private def validateAstType(tpe: ast.Type, typeParams: Set[String], exports: ExportEnv): List[TypeError] =
    tpe match
      case ast.Type.Name(value) =>
        if typeParams.contains(value) || builtinTypeNames.contains(value) || exports.types.contains(value) then Nil
        else List(errorAt(tpe.source, s"Unknown type name: $value"))
      case ast.Type.App(base, args) =>
        validateAstType(base, typeParams, exports) ++ args.flatMap(arg => validateAstType(arg, typeParams, exports))
      case ast.Type.Paren(inner) =>
        validateAstType(inner, typeParams, exports)
      case ast.Type.Fun(param, result) =>
        validateAstType(param, typeParams, exports) ++ validateAstType(result, typeParams, exports)

  private def validateTypedType(
      tpe: Type,
      typeParams: Set[String],
      exports: ExportEnv,
      source: ast.SourceRange
    ): List[TypeError] =
    tpe match
      case Type.Name(value) =>
        if typeParams.contains(value) || builtinTypeNames.contains(value) || exports.types.contains(value) then Nil
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

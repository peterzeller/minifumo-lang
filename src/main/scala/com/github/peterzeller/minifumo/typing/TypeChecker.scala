package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypedAst.*

import scala.collection.mutable.ListBuffer

object TypeChecker:
  final case class TypeError(message: String)

  final case class DataType(name: String, typeParams: List[String], ctors: List[CtorSymbol])

  final case class ExportEnv(
      functions: Map[String, FunctionSymbol],
      ctors: Map[String, CtorSymbol],
      types: Map[String, DataType]
    )

  final case class TypeEnv(
      scopes: List[Map[String, TermSymbol]],
      exports: ExportEnv,
      typeParams: Set[String]
    ):
    def withBinding(symbol: TermSymbol): TypeEnv =
      scopes match
        case head :: tail => copy(scopes = (head + (symbol.name -> symbol)) :: tail)
        case Nil => copy(scopes = List(Map(symbol.name -> symbol)))

    def resolveLocal(name: String): Option[TermSymbol] =
      scopes.collectFirst { case scope if scope.contains(name) => scope(name) }

  private final class IdSupply:
    private var nextId: Int = 1
    def freshId(): Int =
      val id = nextId
      nextId += 1
      id

  private val builtinTypeNames: Set[String] =
    Set("Int", "Bool", "String", "List", "Set", "Map", "unit")

  private val baseTypes: Map[String, Type] =
    Map(
      "Int" -> Type.Name("Int"),
      "Bool" -> Type.Name("Bool"),
      "String" -> Type.Name("String"),
      "unit" -> Type.Name("unit")
    )

  private val baseValues: Map[String, BuiltinValueSymbol] =
    Map(
      "unit" -> BuiltinValueSymbol("unit", baseTypes("unit")),
      "undefined" -> BuiltinValueSymbol("undefined", Type.Unknown),
      "true" -> BuiltinValueSymbol("true", baseTypes("Bool")),
      "false" -> BuiltinValueSymbol("false", baseTypes("Bool"))
    )

  private val baseFunctions: Map[String, BuiltinFunctionSymbol] =
    Map(
      "println" -> BuiltinFunctionSymbol("println", Type.Fun(List(Type.Unknown), baseTypes("unit"))),
      "+" -> BuiltinFunctionSymbol("+", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Int"))),
      "-" -> BuiltinFunctionSymbol("-", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Int"))),
      "*" -> BuiltinFunctionSymbol("*", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Int"))),
      "/" -> BuiltinFunctionSymbol("/", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Int"))),
      "%" -> BuiltinFunctionSymbol("%", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Int"))),
      "<" -> BuiltinFunctionSymbol("<", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Bool"))),
      "<=" -> BuiltinFunctionSymbol("<=", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Bool"))),
      ">" -> BuiltinFunctionSymbol(">", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Bool"))),
      ">=" -> BuiltinFunctionSymbol(">=", Type.Fun(List(baseTypes("Int"), baseTypes("Int")), baseTypes("Bool"))),
      "==" -> BuiltinFunctionSymbol("==", Type.Fun(List(Type.Unknown, Type.Unknown), baseTypes("Bool"))),
      "!=" -> BuiltinFunctionSymbol("!=", Type.Fun(List(Type.Unknown, Type.Unknown), baseTypes("Bool"))),
      "and" -> BuiltinFunctionSymbol("and", Type.Fun(List(baseTypes("Bool"), baseTypes("Bool")), baseTypes("Bool"))),
      "or" -> BuiltinFunctionSymbol("or", Type.Fun(List(baseTypes("Bool"), baseTypes("Bool")), baseTypes("Bool"))),
      "." -> BuiltinFunctionSymbol(".", Type.Fun(List(Type.Unknown, Type.Unknown), Type.Unknown)),
      "block" -> BuiltinFunctionSymbol("block", Type.Fun(Nil, Type.Unknown))
    )

  def checkProgram(program: ast.Program): (Program, List[TypeError]) =
    val (exports, exportErrors) = extractExports(program)
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
          CtorDecl(symbol, ctor.fields.map(f => CtorField(f.name, fromAstType(f.tpe))))
        }
        TopLevel.DataDecl(dataDecl.name, dataDecl.typeParams, ctorDecls)
    }
    (Program(typedItems), errors.toList)

  def extractExports(program: ast.Program): (ExportEnv, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    var functions = Map.empty[String, FunctionSymbol]
    var ctors = Map.empty[String, CtorSymbol]
    var types = Map.empty[String, DataType]

    program.items.foreach {
      case ast.TopLevel.DataDecl(name, typeParams, ctorDecls) =>
        if types.contains(name) then
          errors += TypeError(s"Duplicate data type: $name")
        val typeParamsTypes = typeParams.map(Type.Name.apply)
        val resultType =
          if typeParamsTypes.isEmpty then Type.Name(name) else Type.App(Type.Name(name), typeParamsTypes)
        val ctorSymbols = ctorDecls.map { ctor =>
          val fieldTypes = ctor.fields.map(f => fromAstType(f.tpe))
          val ctorType: Type.Fun = Type.Fun(fieldTypes, resultType)
          CtorSymbol(ctor.name, ctorType, fieldTypes.length, resultType)
        }
        val ctorByName = ctorSymbols.groupBy(_.name)
        ctorByName.collect { case (ctorName, syms) if syms.length > 1 =>
          errors += TypeError(s"Duplicate constructor: $ctorName")
        }
        ctorSymbols.foreach { ctorSymbol =>
          if ctors.contains(ctorSymbol.name) then
            errors += TypeError(s"Duplicate constructor: ${ctorSymbol.name}")
          else
            ctors = ctors + (ctorSymbol.name -> ctorSymbol)
        }
        types = types + (name -> DataType(name, typeParams, ctorSymbols))
        ctorDecls.foreach { ctor =>
          ctor.fields.foreach { field =>
            errors ++= validateType(fromAstType(field.tpe), typeParams.toSet, ExportEnv(functions, ctors, types))
          }
        }
      case ast.TopLevel.FunDecl(name, typeParams, params, returnType, _) =>
        if functions.contains(name) then
          errors += TypeError(s"Duplicate function: $name")
        val paramTypes = params.map(p => fromAstType(p.tpe))
        val returnTpe = returnType.map(fromAstType).getOrElse {
          errors += TypeError(s"Missing return type for function: $name")
          Type.Unknown
        }
        val funSymbol = FunctionSymbol(name, Type.Fun(paramTypes, returnTpe))
        functions = functions + (name -> funSymbol)
        val typeParamSet = typeParams.toSet
        paramTypes.foreach { tpe =>
          errors ++= validateType(tpe, typeParamSet, ExportEnv(functions, ctors, types))
        }
        errors ++= validateType(returnTpe, typeParamSet, ExportEnv(functions, ctors, types))
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
      FunctionSymbol(funDecl.name, Type.Fun(Nil, Type.Unknown))
    )
    val params = funDecl.params.map { param =>
      ParamSymbol(param.name, fromAstType(param.tpe), idSupply.freshId())
    }
    val duplicateParams = params.groupBy(_.name).collect { case (n, ps) if ps.length > 1 => n }
    duplicateParams.foreach { name =>
      errors += TypeError(s"Duplicate parameter: $name")
    }
    val env = TypeEnv(List(params.map(p => p.name -> p).toMap), exports, funDecl.typeParams.toSet)
    val (typedBody, bodyErrors) = typeSuite(funDecl.body, env, funSymbol.tpe.result, idSupply)
    errors ++= bodyErrors
    if !isCompatible(funSymbol.tpe.result, suiteType(typedBody)) then
      errors += TypeError(
        s"Function ${funDecl.name} returns ${renderType(suiteType(typedBody))}, expected ${renderType(funSymbol.tpe.result)}"
      )
    val typedFun: TopLevel.FunDecl = TopLevel.FunDecl(funSymbol, funDecl.typeParams, params, typedBody)
    (typedFun, errors.toList)

  private def typeSuite(
      suite: ast.Suite,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Suite, List[TypeError]) =
    suite match
      case ast.Suite.Single(expr) =>
        val (typedExpr, errors) = typeExpr(expr, env, expectedReturn, idSupply)
        (Suite.Single(typedExpr), errors)
      case ast.Suite.Block(exprs) =>
        val errors = ListBuffer.empty[TypeError]
        val typedExprs = exprs.map { expr =>
          val (typedExpr, exprErrors) = typeExpr(expr, env, expectedReturn, idSupply)
          errors ++= exprErrors
          typedExpr
        }
        val tpe = typedExprs.lastOption.map(_.tpe).getOrElse(baseTypes("unit"))
        (Suite.Block(typedExprs, tpe), errors.toList)

  private def typeExpr(
      expr: ast.Expr,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    expr match
      case ast.Expr.Lit(value) =>
        val tpe = literalType(value)
        (Expr.Lit(value, tpe), Nil)
      case ast.Expr.Var(name) =>
        resolveSymbol(name, env) match
          case Some(symbol) =>
            (Expr.Var(symbol, symbol.tpe), Nil)
          case None =>
            val symbol = ErrorSymbol(name, Type.Unknown)
            (Expr.Var(symbol, symbol.tpe), List(TypeError(s"Unknown symbol: $name")))
      case ast.Expr.Paren(inner) =>
        val (typedInner, errors) = typeExpr(inner, env, expectedReturn, idSupply)
        (Expr.Paren(typedInner, typedInner.tpe), errors)
      case ast.Expr.Call(callee, args) =>
        val (typedCallee, calleeErrors) = typeExpr(callee, env, expectedReturn, idSupply)
        val (typedArgs, argsErrors) = typeExprs(args, env, expectedReturn, idSupply)
        val (resultType, callErrors) = callResultType(typedCallee, typedArgs)
        (Expr.Call(typedCallee, typedArgs, resultType), calleeErrors ++ argsErrors ++ callErrors)
      case ast.Expr.LetIn(name, isConstant, declaredTypeAst, valueExpr, bodyExpr) =>
        val (typedValue, valueErrors) = typeExpr(valueExpr, env, expectedReturn, idSupply)
        val declaredType = declaredTypeAst.map(fromAstType)
        val typeErrors = declaredType.toList.flatMap(tpe => validateType(tpe, env.typeParams, env.exports))
        val bindingType = declaredType.getOrElse(typedValue.tpe)
        val bindingErrors =
          declaredType.toList.flatMap { tpe =>
            if isCompatible(tpe, typedValue.tpe) then Nil
            else List(
              TypeError(
                s"Let-bound value $name has type ${renderType(typedValue.tpe)}, expected ${renderType(tpe)}"
              )
            )
          }
        val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
        val (typedBody, bodyErrors) = typeExpr(bodyExpr, env.withBinding(symbol), expectedReturn, idSupply)
        val allErrors = valueErrors ++ typeErrors ++ bindingErrors ++ bodyErrors
        (Expr.LetIn(symbol, isConstant, declaredType, typedValue, typedBody, typedBody.tpe), allErrors)
      case ast.Expr.IfThenElse(cond, thenExpr, elseExpr) =>
        val (typedCond, condErrors) = typeExpr(cond, env, expectedReturn, idSupply)
        val (typedThen, thenErrors) = typeExpr(thenExpr, env, expectedReturn, idSupply)
        val (typedElse, elseErrors) = typeExpr(elseExpr, env, expectedReturn, idSupply)
        val typeErrors =
          if isCompatible(baseTypes("Bool"), typedCond.tpe) then Nil
          else List(TypeError(s"If condition must be Bool, got ${renderType(typedCond.tpe)}"))
        val (resultType, branchErrors) = unifyBranchTypes("if", typedThen.tpe, typedElse.tpe)
        (
          Expr.IfThenElse(typedCond, typedThen, typedElse, resultType),
          condErrors ++ thenErrors ++ elseErrors ++ typeErrors ++ branchErrors
        )
      case ast.Expr.For(name, inExpr, body) =>
        val (typedIn, inErrors) = typeExpr(inExpr, env, expectedReturn, idSupply)
        val (elemType, elemErrors) = iterableElementType(typedIn.tpe)
        val symbol = LocalSymbol(name, elemType, idSupply.freshId())
        val (typedBody, bodyErrors) = typeSuite(body, env.withBinding(symbol), expectedReturn, idSupply)
        val resultType = suiteType(typedBody)
        (
          Expr.For(symbol, typedIn, typedBody, resultType),
          inErrors ++ elemErrors ++ bodyErrors
        )
      case ast.Expr.While(cond, body) =>
        val (typedCond, condErrors) = typeExpr(cond, env, expectedReturn, idSupply)
        val (typedBody, bodyErrors) = typeSuite(body, env, expectedReturn, idSupply)
        val typeErrors =
          if isCompatible(baseTypes("Bool"), typedCond.tpe) then Nil
          else List(TypeError(s"While condition must be Bool, got ${renderType(typedCond.tpe)}"))
        (Expr.While(typedCond, typedBody, baseTypes("unit")), condErrors ++ bodyErrors ++ typeErrors)
      case ast.Expr.Match(scrutinee, cases) =>
        val (typedScrutinee, scrutineeErrors) = typeExpr(scrutinee, env, expectedReturn, idSupply)
        val errors = ListBuffer.empty[TypeError]
        errors ++= scrutineeErrors
        val typedCases = cases.map { matchCase =>
          val (typedPattern, bindings, patternErrors) =
            typePattern(matchCase.pattern, typedScrutinee.tpe, env, idSupply)
          errors ++= patternErrors
          val envWithBindings = bindings.values.foldLeft(env) { case (current, symbol) => current.withBinding(symbol) }
          val (typedBody, bodyErrors) = typeSuite(matchCase.body, envWithBindings, expectedReturn, idSupply)
          errors ++= bodyErrors
          MatchCase(typedPattern, typedBody)
        }
        val caseTypes = typedCases.map(c => suiteType(c.body))
        val resultType = caseTypes.reduceOption(unifyTypes).getOrElse(Type.Unknown)
        val mismatchErrors = caseTypes.sliding(2).collect {
          case List(a, b) if !isCompatible(a, b) =>
            TypeError(s"Match case types do not agree: ${renderType(a)} vs ${renderType(b)}")
        }.toList
        errors ++= mismatchErrors
        (Expr.Match(typedScrutinee, typedCases, resultType), errors.toList)
      case ast.Expr.Return(valueExpr) =>
        val (typedValue, valueErrors) = typeExpr(valueExpr, env, expectedReturn, idSupply)
        val typeErrors =
          if isCompatible(expectedReturn, typedValue.tpe) then Nil
          else List(
            TypeError(
              s"Return type ${renderType(typedValue.tpe)} does not match expected ${renderType(expectedReturn)}"
            )
          )
        val tpe = if expectedReturn == Type.Unknown then typedValue.tpe else expectedReturn
        (Expr.Return(typedValue, tpe), valueErrors ++ typeErrors)

  private def typeExprs(
      exprs: List[ast.Expr],
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (List[Expr], List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val typed = exprs.map { expr =>
      val (typedExpr, exprErrors) = typeExpr(expr, env, expectedReturn, idSupply)
      errors ++= exprErrors
      typedExpr
    }
    (typed, errors.toList)

  private def typePattern(
      pattern: ast.Pattern,
      expectedType: Type,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Pattern, Map[String, TermSymbol], List[TypeError]) =
    pattern match
      case ast.Pattern.Wildcard =>
        (Pattern.Wildcard, Map.empty, Nil)
      case ast.Pattern.Lit(value) =>
        val litType = literalType(value)
        val errors =
          if isCompatible(expectedType, litType) then Nil
          else List(
            TypeError(
              s"Pattern literal has type ${renderType(litType)}, expected ${renderType(expectedType)}"
            )
          )
        (Pattern.Lit(value), Map.empty, errors)
      case ast.Pattern.BinderOrCtor0(name) =>
        env.exports.ctors.get(name) match
          case Some(ctor) if ctor.arity == 0 =>
            val errors =
              if isCompatible(expectedType, ctor.resultType) then Nil
              else List(
                TypeError(
                  s"Constructor $name has type ${renderType(ctor.resultType)}, expected ${renderType(expectedType)}"
                )
              )
            (Pattern.Ctor(ctor, Nil), Map.empty, errors)
          case _ =>
            val symbol = LocalSymbol(name, expectedType, idSupply.freshId())
            (Pattern.Binder(symbol), Map(name -> symbol), Nil)
      case ast.Pattern.Ctor(name, args) =>
        env.exports.ctors.get(name) match
          case Some(ctor) =>
            val Type.Fun(paramTypes, resultType) = ctor.tpe
            val errors = ListBuffer.empty[TypeError]
            if ctor.arity != args.length then
              errors += TypeError(s"Constructor $name expects ${ctor.arity} args, got ${args.length}")
            if !isCompatible(expectedType, resultType) then
              errors += TypeError(
                s"Constructor $name returns ${renderType(resultType)}, expected ${renderType(expectedType)}"
              )
            val typedArgs = args.zipAll(paramTypes, ast.Pattern.Wildcard, Type.Unknown).map { case (arg, tpe) =>
              val (typedArg, bindings, argErrors) = typePattern(arg, tpe, env, idSupply)
              errors ++= argErrors
              (typedArg, bindings)
            }
            val bindings = mergeBindings(typedArgs.map(_._2), errors)
            (Pattern.Ctor(ctor, typedArgs.map(_._1)), bindings, errors.toList)
          case None =>
            val unknownCtor = CtorSymbol(name, Type.Fun(Nil, Type.Unknown), 0, Type.Unknown)
            (
              Pattern.Ctor(unknownCtor, Nil),
              Map.empty,
              List(TypeError(s"Unknown constructor: $name"))
            )

  private def mergeBindings(
      bindingSets: List[Map[String, TermSymbol]],
      errors: ListBuffer[TypeError]
    ): Map[String, TermSymbol] =
    bindingSets.foldLeft(Map.empty[String, TermSymbol]) { (acc, bindings) =>
      bindings.foreach { case (name, symbol) =>
        if acc.contains(name) then
          errors += TypeError(s"Duplicate pattern binder: $name")
      }
      acc ++ bindings
    }

  private def callResultType(callee: Expr, args: List[Expr]): (Type, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val calleeType = callee.tpe
    callee match
      case Expr.Var(symbol, _) if symbol.name == "block" =>
        val resultType = args.lastOption.map(_.tpe).getOrElse(baseTypes("unit"))
        (resultType, Nil)
      case Expr.Var(symbol, _) if symbol.name == "." && args.length == 2 =>
        errors += TypeError("Field access typing is not implemented")
        (Type.Unknown, errors.toList)
      case Expr.Var(symbol, _) if symbol.name == "-" && args.length == 1 =>
        if args.headOption.exists(arg => !isCompatible(baseTypes("Int"), arg.tpe)) then
          errors += TypeError(s"Unary - expects Int, got ${renderType(args.head.tpe)}")
        (baseTypes("Int"), errors.toList)
      case _ =>
        calleeType match
          case Type.Fun(params, result) =>
            if params.length != args.length then
              errors += TypeError(s"Call expects ${params.length} args, got ${args.length}")
            params.zipAll(args, Type.Unknown, Expr.Var(ErrorSymbol("<missing>", Type.Unknown), Type.Unknown)).foreach {
              case (expected, actual) if !isCompatible(expected, actual.tpe) =>
                errors += TypeError(
                  s"Argument has type ${renderType(actual.tpe)}, expected ${renderType(expected)}"
                )
              case _ => ()
            }
            (result, errors.toList)
          case _ =>
            errors += TypeError(s"Call target is not a function: ${renderType(calleeType)}")
            (Type.Unknown, errors.toList)

  private def iterableElementType(tpe: Type): (Type, List[TypeError]) =
    tpe match
      case Type.App(Type.Name("List"), List(elem)) => (elem, Nil)
      case Type.App(Type.Name("Set"), List(elem)) => (elem, Nil)
      case Type.App(Type.Name("Map"), List(key, _)) => (key, Nil)
      case Type.Unknown => (Type.Unknown, Nil)
      case _ => (Type.Unknown, List(TypeError(s"Expected iterable, got ${renderType(tpe)}")))

  private def suiteType(suite: Suite): Type =
    suite match
      case Suite.Single(expr) => expr.tpe
      case Suite.Block(_, tpe) => tpe

  private def unifyBranchTypes(
      context: String,
      left: Type,
      right: Type
    ): (Type, List[TypeError]) =
    if isCompatible(left, right) then (unifyTypes(left, right), Nil)
    else
      (
        Type.Unknown,
        List(TypeError(s"$context branches have incompatible types: ${renderType(left)} vs ${renderType(right)}"))
      )

  private def unifyTypes(left: Type, right: Type): Type =
    (left, right) match
      case (Type.Unknown, other) => other
      case (other, Type.Unknown) => other
      case _ if left == right => left
      case _ => Type.Unknown

  private def isCompatible(expected: Type, actual: Type): Boolean =
    (expected, actual) match
      case (Type.Unknown, _) => true
      case (_, Type.Unknown) => true
      case _ => expected == actual

  private def resolveSymbol(name: String, env: TypeEnv): Option[Symbol] =
    env.resolveLocal(name).orElse(baseValues.get(name)).orElse(env.exports.functions.get(name)).orElse(
      env.exports.ctors.get(name)
    ).orElse(baseFunctions.get(name))

  private def fromAstType(tpe: ast.Type): Type =
    tpe match
      case ast.Type.Name(value) => Type.Name(value)
      case ast.Type.Paren(inner) => fromAstType(inner)
      case ast.Type.App(base, args) => Type.App(fromAstType(base), args.map(fromAstType))

  private def validateType(tpe: Type, typeParams: Set[String], exports: ExportEnv): List[TypeError] =
    tpe match
      case Type.Name(value) =>
        if typeParams.contains(value) || builtinTypeNames.contains(value) || exports.types.contains(value) then Nil
        else List(TypeError(s"Unknown type name: $value"))
      case Type.App(base, args) =>
        validateType(base, typeParams, exports) ++ args.flatMap(arg => validateType(arg, typeParams, exports))
      case Type.Fun(params, result) =>
        params.flatMap(param => validateType(param, typeParams, exports)) ++ validateType(result, typeParams, exports)
      case Type.Unknown => Nil

  private def literalType(literal: ast.Literal): Type =
    literal match
      case ast.Literal.IntLit(_) => baseTypes("Int")
      case ast.Literal.BoolLit(_) => baseTypes("Bool")
      case ast.Literal.StringLit(_) => baseTypes("String")

  private def renderType(tpe: Type): String =
    tpe match
      case Type.Name(value) => value
      case Type.App(base, args) => s"${renderType(base)}[${args.map(renderType).mkString(", ")}]"
      case Type.Fun(params, result) =>
        s"(${params.map(renderType).mkString(", ")}) -> ${renderType(result)}"
      case Type.Unknown => "Unknown"

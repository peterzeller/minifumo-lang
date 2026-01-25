package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
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

  final case class TypeEnv(
      scopes: List[Map[String, TermSymbol]],
      exports: ExportEnv,
      typeParams: Set[String],
      expectedReturn: Type
    ):
    def withBinding(symbol: TermSymbol): TypeEnv =
      scopes match
        case head :: tail => copy(scopes = (head + (symbol.name -> symbol)) :: tail)
        case Nil => copy(scopes = List(Map(symbol.name -> symbol)))

    def resolveLocal(name: String): Option[TermSymbol] =
      scopes.collectFirst { case scope if scope.contains(name) => scope(name) }

  private def errorAt(source: ast.SourceRange, message: String): TypeError =
    TypeError(message, source)

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
      "." -> BuiltinFunctionSymbol(".", Type.Fun(List(Type.Unknown, Type.Unknown), Type.Unknown))
    )

  def checkProgram(program: ast.ProgramFile): (Program, List[TypeError]) =
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
          CtorDecl(
            symbol,
            ctor.fields.map(f => CtorField(f.name, fromAstType(f.tpe))(f.source))
          )(ctor.source)
        }
        TopLevel.DataDecl(dataDecl.name, dataDecl.typeParams, ctorDecls)(dataDecl.source)
    }
    (Program(typedItems)(program.source), errors.toList)

  def extractExports(program: ast.ProgramFile): (ExportEnv, List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    var functions = Map.empty[String, FunctionSymbol]
    var ctors = Map.empty[String, CtorSymbol]
    var types = Map.empty[String, DataType]

    for item <- program.items do {
      item match
      case ast.TopLevel.DataDecl(name, typeParams, ctorDecls) =>
        if types.contains(name) then
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
          if ctors.contains(ctorSymbol.name) then
            errors += errorAt(ctorDecl.source, s"Duplicate constructor: ${ctorSymbol.name}")
          else
            ctors = ctors + (ctorSymbol.name -> ctorSymbol)
        }
        types = types + (name -> DataType(name, typeParams, ctorSymbols))
        ctorDecls.foreach { ctor =>
          ctor.fields.foreach { field =>
            errors ++= validateAstType(field.tpe, typeParams.toSet, ExportEnv(functions, ctors, types))
          }
        }
      case ast.TopLevel.FunDecl(name, typeParams, params, returnType, _) =>
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
    val env =
      TypeEnv(List(params.map(p => p.name -> p).toMap), exports, funDecl.typeParams.toSet, funSymbol.tpe.result)
    val expectedBodyType =
      if funSymbol.tpe.result == Type.Unknown then None else Some(funSymbol.tpe.result)
    val (typedBody, bodyErrors) = typeSuite(funDecl.body, env, funSymbol.tpe.result, expectedBodyType, idSupply)
    errors ++= bodyErrors
    if !isCompatible(funSymbol.tpe.result, suiteType(typedBody)) then
      errors += errorAt(
        funDecl.source,
        s"Function ${funDecl.name} returns ${renderType(suiteType(typedBody))}, expected ${renderType(funSymbol.tpe.result)}"
      )
    val typedFun: TopLevel.FunDecl = TopLevel.FunDecl(funSymbol, funDecl.typeParams, params, typedBody)(funDecl.source)
    (typedFun, errors.toList)

  private def typeSuite(
      suite: ast.Suite,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (Suite, List[TypeError]) =
    suite match
      case ast.Suite.Single(expr) =>
        val (typedExpr, errors) = typeExpr(expr, env, expectedReturn, expectedType, idSupply)
        (Suite.Single(typedExpr)(suite.source), errors)
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
        (Suite.Block(typedExprs, tpe)(suite.source), errors.toList)

  private def typeExpr(
      expr: ast.Expr,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
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
    ): (Expr, List[TypeError]) =
    expr match
      case ast.Expr.Lit(value) => synthLit(value, expr.source)
      case ast.Expr.Var(name) => synthVar(name, expr.source, env)
      case ast.Expr.Paren(inner) => synthParen(inner, expr.source, env, idSupply)
      case ast.Expr.Block(exprs) => synthBlock(exprs, expr.source, env, idSupply)
      case ast.Expr.Call(callee, args) => synthCall(callee, args, expr.source, env, idSupply)
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
      case ast.Expr.Return(valueExpr) =>
        val symbol = ErrorSymbol("<return>", Type.Unknown)
        (
          Expr.Return(Expr.Var(symbol, Type.Unknown)(expr.source), Type.Unknown)(expr.source),
          List(errorAt(expr.source, "Return expression requires an expected return type"))
        )

  private def checkExpr(
      expr: ast.Expr,
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    expr match
      case ast.Expr.Lit(value) => checkLit(value, expectedType, expr.source)
      case ast.Expr.Var(name) => checkVar(name, expectedType, expr.source, env)
      case ast.Expr.Paren(inner) => checkParen(inner, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Block(exprs) =>
        checkBlock(exprs, expectedType, expr.source, env, expectedReturn, idSupply)
      case ast.Expr.Call(callee, args) =>
        checkCall(callee, args, expectedType, expr.source, env, expectedReturn, idSupply)
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
  private def synthLit(value: ast.Literal, source: ast.SourceRange): (Expr, List[TypeError]) =
    val tpe = literalType(value)
    (Expr.Lit(value, tpe)(source), Nil)

  // T-Lit-Check: Γ ⊢ e ⇐ T  if  Γ ⊢ e ⇒ S  and  S ≈ T
  private def checkLit(value: ast.Literal, expectedType: Type, source: ast.SourceRange): (Expr, List[TypeError]) =
    val tpe = literalType(value)
    val (checkedType, errors) = ensureExpectedType(tpe, Some(expectedType), source)
    (Expr.Lit(value, checkedType)(source), errors)

  // T-Var: Γ(x) = T  ⇒  Γ ⊢ x ⇒ T
  private def synthVar(name: String, source: ast.SourceRange, env: TypeEnv): (Expr, List[TypeError]) =
    resolveSymbol(name, env) match
      case Some(symbol: CtorSymbol) if symbol.arity == 0 =>
        (Expr.CallCtor(symbol, Nil, symbol.resultType)(source), Nil)
      case Some(symbol) =>
        (Expr.Var(symbol, symbol.tpe)(source), Nil)
      case None =>
        val symbol = ErrorSymbol(name, Type.Unknown)
        (Expr.Var(symbol, symbol.tpe)(source), List(errorAt(source, s"Unknown symbol: $name")))

  // T-Var-Check: Γ(x) = T  ⇒  Γ ⊢ x ⇐ T'
  private def checkVar(
      name: String,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv
    ): (Expr, List[TypeError]) =
    resolveSymbol(name, env) match
      case Some(symbol: CtorSymbol) if symbol.arity == 0 =>
        val tpe = if expectedType != Type.Unknown then
          val typeParamBindings = scala.collection.mutable.Map.empty[String, Type]
          collectTypeParamBindings(symbol.resultType, expectedType, symbol.typeParams.toSet, typeParamBindings)
          instantiateType(symbol.resultType, typeParamBindings.toMap)
        else symbol.resultType
        val (checkedType, errors) = ensureExpectedType(tpe, Some(expectedType), source)
        (Expr.CallCtor(symbol, Nil, checkedType)(source), errors)
      case Some(symbol) =>
        val (checkedType, errors) = ensureExpectedType(symbol.tpe, Some(expectedType), source)
        (Expr.Var(symbol, checkedType)(source), errors)
      case None =>
        val symbol = ErrorSymbol(name, Type.Unknown)
        val (checkedType, errors) = ensureExpectedType(symbol.tpe, Some(expectedType), source)
        (Expr.Var(symbol, checkedType)(source), errors :+ errorAt(source, s"Unknown symbol: $name"))

  // T-Paren: Γ ⊢ e ⇒ T  ⇒  Γ ⊢ (e) ⇒ T
  private def synthParen(
      inner: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedInner, errors) = synthesizeExpr(inner, env, idSupply)
    (Expr.Paren(typedInner, typedInner.tpe)(source), errors)

  // T-Paren-Check: Γ ⊢ e ⇐ T  ⇒  Γ ⊢ (e) ⇐ T
  private def checkParen(
      inner: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedInner, errors) = checkExpr(inner, env, expectedReturn, expectedType, idSupply)
    (Expr.Paren(typedInner, typedInner.tpe)(source), errors)

  private def synthBlock(
      exprs: List[ast.Expr],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    typeBlockExpr(exprs, None, source, env, env.expectedReturn, idSupply)

  private def checkBlock(
      exprs: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    typeBlockExpr(exprs, Some(expectedType), source, env, expectedReturn, idSupply)

  private def typeBlockExpr(
      exprs: List[ast.Expr],
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
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
    (Expr.Block(typedExprs, tpe)(source), errors.toList)

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
        val (typedExpr, errors) = expectedType match
          case Some(tpe) => checkAssign(name, valueExpr, tpe, expr.source, env, expectedReturn, idSupply)
          case None => synthAssign(name, valueExpr, expr.source, env, idSupply)
        (typedExpr, errors, env)
      case _ =>
        val (typedExpr, errors) = expectedType match
          case Some(tpe) => checkExpr(expr, env, expectedReturn, tpe, idSupply)
          case None => synthesizeExpr(expr, env, idSupply)
        (typedExpr, errors, env)

  // T-App: Γ ⊢ f ⇒ T1 → T2  and  Γ ⊢ a ⇐ T1  ⇒  Γ ⊢ f a ⇒ T2
  private def synthCall(
      callee: ast.Expr,
      args: List[ast.Expr],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    checkCall(callee, args, Type.Unknown, source, env, env.expectedReturn, idSupply)

  // T-App-Check: Γ ⊢ f ⇒ T1 → T2  and  Γ ⊢ a ⇐ T1  and  T2 ≈ T  ⇒  Γ ⊢ f a ⇐ T
  private def checkCall(
      callee: ast.Expr,
      args: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedCallee, calleeErrors) = synthesizeExpr(callee, env, idSupply)
    typedCallee match
      case Expr.Var(symbol, _) if symbol.name == "." && args.length == 2 =>
        val (typedArgs, argsErrors) = typeExprs(args, env, expectedReturn, None, idSupply)
        val errors = calleeErrors ++ argsErrors :+ errorAt(callee.source, "Field access typing is not implemented")
        (Expr.CallFun(typedCallee, typedArgs, Type.Unknown)(source), errors)
      case Expr.Var(symbol, _) if symbol.name == "-" && args.length == 1 =>
        val (typedArg, argErrors) = checkExpr(args.head, env, expectedReturn, baseTypes("Int"), idSupply)
        val errors = calleeErrors ++ argErrors
        (Expr.CallFun(typedCallee, List(typedArg), baseTypes("Int"))(source), errors)
      case Expr.Var(symbol: CtorSymbol, _) =>
        val (typedExpr, ctorErrors) = typeCtorCall(symbol, args, expectedType, source, env, expectedReturn, idSupply)
        (typedExpr, calleeErrors ++ ctorErrors)
      case _ =>
        val maybeFunType = resolveFunctionType(typedCallee)
        maybeFunType match
          case Some((funType, typeParams)) =>
            val errors = ListBuffer.empty[TypeError]
            errors ++= calleeErrors
            val typeParamBindings = scala.collection.mutable.Map.empty[String, Type]
            if expectedType != Type.Unknown then
              collectTypeParamBindings(funType.result, expectedType, typeParams, typeParamBindings)
            val typedArgs = args.zipWithIndex.map { case (arg, index) =>
              val expectedParamType =
                if index < funType.params.length then
                  instantiateType(funType.params(index), typeParamBindings.toMap)
                else Type.Unknown
              val (typedArg, argErrors) = checkExpr(arg, env, expectedReturn, expectedParamType, idSupply)
              errors ++= argErrors
              if index < funType.params.length then
                collectTypeParamBindings(funType.params(index), typedArg.tpe, typeParams, typeParamBindings)
              typedArg
            }
            val instantiatedParamTypes = funType.params.map(p => instantiateType(p, typeParamBindings.toMap))
            val instantiatedResultType = instantiateType(funType.result, typeParamBindings.toMap)
            if instantiatedParamTypes.length != args.length then
              errors += errorAt(callee.source, s"Call expects ${instantiatedParamTypes.length} args, got ${args.length}")
            instantiatedParamTypes.zipAll(
              typedArgs,
              Type.Unknown,
              Expr.Var(ErrorSymbol("<missing>", Type.Unknown), Type.Unknown)(callee.source)
            ).foreach {
              case (expected, actual) if !isCompatible(expected, actual.tpe) =>
                errors += errorAt(
                  actual.source,
                  s"Argument has type ${renderType(actual.tpe)}, expected ${renderType(expected)}"
                )
              case _ => ()
            }
            val (checkedType, expectedErrors) = ensureExpectedType(instantiatedResultType, Some(expectedType), source)
            errors ++= expectedErrors
            (Expr.CallFun(typedCallee, typedArgs, checkedType)(source), errors.toList)
          case None =>
            val (typedArgs, argsErrors) = typeExprs(args, env, expectedReturn, None, idSupply)
            val errors =
              calleeErrors ++ argsErrors :+ errorAt(callee.source, s"Call target is not a function: ${renderType(typedCallee.tpe)}")
            (Expr.CallFun(typedCallee, typedArgs, Type.Unknown)(source), errors)

  private def typeCtorCall(
      ctor: CtorSymbol,
      args: List[ast.Expr],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val Type.Fun(paramTypes, resultType) = ctor.tpe
    val errors = ListBuffer.empty[TypeError]
    val typeParamBindings = scala.collection.mutable.Map.empty[String, Type]
    if expectedType != Type.Unknown then
      collectTypeParamBindings(resultType, expectedType, ctor.typeParams.toSet, typeParamBindings)
    val typedArgs = args.zipWithIndex.map { case (arg, index) =>
      val expectedParamType =
        if index < paramTypes.length then
          instantiateType(paramTypes(index), typeParamBindings.toMap)
        else Type.Unknown
      val (typedArg, argErrors) = checkExpr(arg, env, expectedReturn, expectedParamType, idSupply)
      errors ++= argErrors
      if index < paramTypes.length then
        collectTypeParamBindings(paramTypes(index), typedArg.tpe, ctor.typeParams.toSet, typeParamBindings)
      typedArg
    }
    val instantiatedParamTypes = paramTypes.map(p => instantiateType(p, typeParamBindings.toMap))
    val instantiatedResultType = instantiateType(resultType, typeParamBindings.toMap)
    if instantiatedParamTypes.length != args.length then
      errors += errorAt(source, s"Constructor ${ctor.name} expects ${instantiatedParamTypes.length} args, got ${args.length}")
    instantiatedParamTypes.zipAll(
      typedArgs,
      Type.Unknown,
      Expr.Var(ErrorSymbol("<missing>", Type.Unknown), Type.Unknown)(source)
    ).foreach {
      case (expected, actual) if !isCompatible(expected, actual.tpe) =>
        errors += errorAt(
          actual.source,
          s"Argument has type ${renderType(actual.tpe)}, expected ${renderType(expected)}"
        )
      case _ => ()
    }
    val (checkedType, expectedErrors) = ensureExpectedType(instantiatedResultType, Some(expectedType), source)
    errors ++= expectedErrors
    (Expr.CallCtor(ctor, typedArgs, checkedType)(source), errors.toList)

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
    ): (Expr, List[TypeError]) =
    val declaredType = declaredTypeAst.map(fromAstType)
    val (typedValue, valueErrors) = declaredType match
      case Some(tpe) => checkExpr(valueExpr, env, env.expectedReturn, tpe, idSupply)
      case None => synthesizeExpr(valueExpr, env, idSupply)
    val typeErrors = declaredTypeAst.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val bindingType = declaredType.getOrElse(typedValue.tpe)
    val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
    val (typedBody, bodyErrors) = synthesizeExpr(bodyExpr, env.withBinding(symbol), idSupply)
    val allErrors = valueErrors ++ typeErrors ++ bodyErrors
    (Expr.LetIn(symbol, isConstant, declaredType, typedValue, typedBody, typedBody.tpe)(source), allErrors)

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
    ): (Expr, List[TypeError]) =
    val declaredType = declaredTypeAst.map(fromAstType)
    val (typedValue, valueErrors) = declaredType match
      case Some(tpe) => checkExpr(valueExpr, env, env.expectedReturn, tpe, idSupply)
      case None => synthesizeExpr(valueExpr, env, idSupply)
    val typeErrors = declaredTypeAst.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val bindingType = declaredType.getOrElse(typedValue.tpe)
    val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
    val (typedBody, bodyErrors) = checkExpr(bodyExpr, env.withBinding(symbol), expectedReturn, expectedType, idSupply)
    val allErrors = valueErrors ++ typeErrors ++ bodyErrors
    (Expr.LetIn(symbol, isConstant, declaredType, typedValue, typedBody, typedBody.tpe)(source), allErrors)

  private def synthBind(
      name: String,
      isConstant: Boolean,
      declaredTypeAst: Option[ast.Type],
      valueExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedExpr, errors, _) =
      typeBindWithEnv(name, isConstant, declaredTypeAst, valueExpr, Some(baseTypes("unit")), source, env, env.expectedReturn, idSupply)
    (typedExpr, errors)

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
    ): (Expr, List[TypeError]) =
    val (typedExpr, errors, _) =
      typeBindWithEnv(name, isConstant, declaredTypeAst, valueExpr, Some(expectedType), source, env, expectedReturn, idSupply)
    (typedExpr, errors)

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
    val (typedValue, valueErrors) = declaredType match
      case Some(tpe) => checkExpr(valueExpr, env, expectedReturn, tpe, idSupply)
      case None => synthesizeExpr(valueExpr, env, idSupply)
    val typeErrors = declaredTypeAst.toList.flatMap(tpe => validateAstType(tpe, env.typeParams, env.exports))
    val bindingType = declaredType.getOrElse(typedValue.tpe)
    val symbol = LocalSymbol(name, bindingType, idSupply.freshId())
    val (checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source)
    val typedExpr = Expr.Bind(symbol, isConstant, declaredType, typedValue, checkedType)(source)
    (typedExpr, valueErrors ++ typeErrors ++ expectedErrors, env.withBinding(symbol))

  private def synthAssign(
      name: String,
      valueExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedExpr, errors) = typeAssign(name, valueExpr, None, source, env, env.expectedReturn, idSupply)
    (typedExpr, errors)

  private def checkAssign(
      name: String,
      valueExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedExpr, errors) = typeAssign(name, valueExpr, Some(expectedType), source, env, expectedReturn, idSupply)
    (typedExpr, errors)

  private def typeAssign(
      name: String,
      valueExpr: ast.Expr,
      expectedType: Option[Type],
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    env.resolveLocal(name) match
      case Some(symbol: LocalSymbol) =>
        val (typedValue, valueErrors) = checkExpr(valueExpr, env, expectedReturn, symbol.tpe, idSupply)
        val (checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source)
        (Expr.Assign(symbol, typedValue, checkedType)(source), valueErrors ++ expectedErrors)
      case Some(symbol) =>
        val (typedValue, valueErrors) = synthesizeExpr(valueExpr, env, idSupply)
        val (checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source)
        val errors = valueErrors ++ expectedErrors :+ errorAt(source, s"Cannot assign to ${symbol.name}")
        val fallbackSymbol = LocalSymbol(name, Type.Unknown, idSupply.freshId())
        (Expr.Assign(fallbackSymbol, typedValue, checkedType)(source), errors)
      case None =>
        val (typedValue, valueErrors) = synthesizeExpr(valueExpr, env, idSupply)
        val (checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), expectedType, source)
        val symbol = LocalSymbol(name, Type.Unknown, idSupply.freshId())
        (
          Expr.Assign(symbol, typedValue, checkedType)(source),
          valueErrors ++ expectedErrors :+ errorAt(source, s"Unknown variable: $name")
        )

  // T-If: Γ ⊢ c ⇐ Bool  and  Γ ⊢ t ⇒ T  and  Γ ⊢ e ⇒ T  ⇒  Γ ⊢ if c then t else e ⇒ T
  private def synthIfThenElse(
      cond: ast.Expr,
      thenExpr: ast.Expr,
      elseExpr: ast.Expr,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedCond, condErrors) = checkExpr(cond, env, env.expectedReturn, baseTypes("Bool"), idSupply)
    val (typedThen, thenErrors) = synthesizeExpr(thenExpr, env, idSupply)
    val (typedElse, elseErrors) = synthesizeExpr(elseExpr, env, idSupply)
    val (resultType, branchErrors) = unifyBranchTypes("if", typedThen.tpe, typedElse.tpe, source)
    (
      Expr.IfThenElse(typedCond, typedThen, typedElse, resultType)(source),
      condErrors ++ thenErrors ++ elseErrors ++ branchErrors
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
    ): (Expr, List[TypeError]) =
    val (typedCond, condErrors) = checkExpr(cond, env, expectedReturn, baseTypes("Bool"), idSupply)
    val (typedThen, thenErrors) = checkExpr(thenExpr, env, expectedReturn, expectedType, idSupply)
    val (typedElse, elseErrors) = checkExpr(elseExpr, env, expectedReturn, expectedType, idSupply)
    val (resultType, branchErrors) = unifyBranchTypes("if", typedThen.tpe, typedElse.tpe, source)
    (
      Expr.IfThenElse(typedCond, typedThen, typedElse, expectedType)(source),
      condErrors ++ thenErrors ++ elseErrors ++ branchErrors
    )

  // T-For: Γ ⊢ xs ⇒ Iterable[T]  and  Γ,x:T ⊢ e ⇒ U  ⇒  Γ ⊢ for x in xs do e ⇒ U
  private def synthFor(
      name: String,
      inExpr: ast.Expr,
      body: ast.Suite,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedIn, inErrors) = synthesizeExpr(inExpr, env, idSupply)
    val (elemType, elemErrors) = iterableElementType(typedIn.tpe, typedIn.source)
    val symbol = LocalSymbol(name, elemType, idSupply.freshId())
    val (typedBody, bodyErrors) = typeSuite(body, env.withBinding(symbol), env.expectedReturn, None, idSupply)
    val resultType = suiteType(typedBody)
    (
      Expr.For(symbol, typedIn, typedBody, resultType)(source),
      inErrors ++ elemErrors ++ bodyErrors
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
    ): (Expr, List[TypeError]) =
    val (typedIn, inErrors) = synthesizeExpr(inExpr, env, idSupply)
    val (elemType, elemErrors) = iterableElementType(typedIn.tpe, typedIn.source)
    val symbol = LocalSymbol(name, elemType, idSupply.freshId())
    val (typedBody, bodyErrors) = typeSuite(body, env.withBinding(symbol), expectedReturn, Some(expectedType), idSupply)
    (
      Expr.For(symbol, typedIn, typedBody, expectedType)(source),
      inErrors ++ elemErrors ++ bodyErrors
    )

  // T-While: Γ ⊢ c ⇐ Bool  and  Γ ⊢ e ⇒ unit  ⇒  Γ ⊢ while c do e ⇒ unit
  private def synthWhile(
      cond: ast.Expr,
      body: ast.Suite,
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedCond, condErrors) = checkExpr(cond, env, env.expectedReturn, baseTypes("Bool"), idSupply)
    val (typedBody, bodyErrors) = typeSuite(body, env, env.expectedReturn, None, idSupply)
    val (checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), Some(baseTypes("unit")), source)
    (
      Expr.While(typedCond, typedBody, checkedType)(source),
      condErrors ++ bodyErrors ++ expectedErrors
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
    ): (Expr, List[TypeError]) =
    val (typedCond, condErrors) = checkExpr(cond, env, expectedReturn, baseTypes("Bool"), idSupply)
    val (typedBody, bodyErrors) = typeSuite(body, env, expectedReturn, Some(baseTypes("unit")), idSupply)
    val (checkedType, expectedErrors) = ensureExpectedType(baseTypes("unit"), Some(expectedType), source)
    (
      Expr.While(typedCond, typedBody, checkedType)(source),
      condErrors ++ bodyErrors ++ expectedErrors
    )

  // T-Match: Γ ⊢ e ⇒ T and Γ ⊢ cases ⇒ U  ⇒  Γ ⊢ match e with cases ⇒ U
  private def synthMatch(
      scrutinee: ast.Expr,
      cases: List[ast.MatchCase],
      source: ast.SourceRange,
      env: TypeEnv,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedScrutinee, scrutineeErrors) = synthesizeExpr(scrutinee, env, idSupply)
    val errors = ListBuffer.empty[TypeError]
    errors ++= scrutineeErrors
    val typedCases = cases.map { matchCase =>
      val (typedPattern, bindings, patternErrors) =
        typePattern(matchCase.pattern, typedScrutinee.tpe, env, idSupply)
      errors ++= patternErrors
      val envWithBindings = bindings.values.foldLeft(env) { case (current, symbol) => current.withBinding(symbol) }
      val (typedBody, bodyErrors) = typeSuite(matchCase.body, envWithBindings, env.expectedReturn, None, idSupply)
      errors ++= bodyErrors
      MatchCase(typedPattern, typedBody)(matchCase.source)
    }
    val caseTypes = typedCases.map(c => suiteType(c.body))
    val resultType = caseTypes.reduceOption(unifyTypes).getOrElse(Type.Unknown)
    val mismatchErrors = caseTypes.sliding(2).collect {
      case List(a, b) if !isCompatible(a, b) =>
        errorAt(source, s"Match case types do not agree: ${renderType(a)} vs ${renderType(b)}")
    }.toList
    errors ++= mismatchErrors
    (Expr.Match(typedScrutinee, typedCases, resultType)(source), errors.toList)

  // T-Match-Check: Γ ⊢ e ⇒ T and Γ ⊢ cases ⇐ U  ⇒  Γ ⊢ match e with cases ⇐ U
  private def checkMatch(
      scrutinee: ast.Expr,
      cases: List[ast.MatchCase],
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val (typedScrutinee, scrutineeErrors) = synthesizeExpr(scrutinee, env, idSupply)
    val errors = ListBuffer.empty[TypeError]
    errors ++= scrutineeErrors
    val typedCases = cases.map { matchCase =>
      val (typedPattern, bindings, patternErrors) =
        typePattern(matchCase.pattern, typedScrutinee.tpe, env, idSupply)
      errors ++= patternErrors
      val envWithBindings = bindings.values.foldLeft(env) { case (current, symbol) => current.withBinding(symbol) }
      val (typedBody, bodyErrors) =
        typeSuite(matchCase.body, envWithBindings, expectedReturn, Some(expectedType), idSupply)
      errors ++= bodyErrors
      MatchCase(typedPattern, typedBody)(matchCase.source)
    }
    val caseTypes = typedCases.map(c => suiteType(c.body))
    val mismatchErrors = caseTypes.sliding(2).collect {
      case List(a, b) if !isCompatible(a, b) =>
        errorAt(source, s"Match case types do not agree: ${renderType(a)} vs ${renderType(b)}")
    }.toList
    errors ++= mismatchErrors
    (Expr.Match(typedScrutinee, typedCases, expectedType)(source), errors.toList)

  // T-Return-Check: Γ ⊢ e ⇐ Tret  ⇒  Γ ⊢ return e ⇐ Tret
  private def checkReturn(
      valueExpr: ast.Expr,
      expectedType: Type,
      source: ast.SourceRange,
      env: TypeEnv,
      expectedReturn: Type,
      idSupply: IdSupply
    ): (Expr, List[TypeError]) =
    val returnExpectedType = if expectedReturn == Type.Unknown then expectedType else expectedReturn
    val (typedValue, valueErrors) = checkExpr(valueExpr, env, expectedReturn, returnExpectedType, idSupply)
    val typeErrors =
      if expectedReturn == Type.Unknown || isCompatible(expectedReturn, typedValue.tpe) then Nil
      else List(
        errorAt(
          source,
          s"Return type ${renderType(typedValue.tpe)} does not match expected ${renderType(expectedReturn)}"
        )
      )
    val tpe = if expectedReturn == Type.Unknown then typedValue.tpe else expectedReturn
    val (checkedType, expectedErrors) = ensureExpectedType(tpe, Some(expectedType), source)
    (Expr.Return(typedValue, checkedType)(source), valueErrors ++ typeErrors ++ expectedErrors)

  private def typeExprs(
      exprs: List[ast.Expr],
      env: TypeEnv,
      expectedReturn: Type,
      expectedType: Option[Type],
      idSupply: IdSupply
    ): (List[Expr], List[TypeError]) =
    val errors = ListBuffer.empty[TypeError]
    val typed = exprs.map { expr =>
      val (typedExpr, exprErrors) = typeExpr(expr, env, expectedReturn, expectedType, idSupply)
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
    val source = pattern.source
    pattern match
      case ast.Pattern.Wildcard() =>
        (Pattern.Wildcard()(pattern.source), Map.empty, Nil)
      case ast.Pattern.Lit(value) =>
        val litType = literalType(value)
        val errors =
          if isCompatible(expectedType, litType) then Nil
          else List(
            errorAt(
              source,
              s"Pattern literal has type ${renderType(litType)}, expected ${renderType(expectedType)}"
            )
          )
        (Pattern.Lit(value)(source), Map.empty, errors)
      case ast.Pattern.BinderOrCtor0(name) =>
        env.exports.ctors.get(name) match
          case Some(ctor) if ctor.arity == 0 =>
            val typeParamBindings = scala.collection.mutable.Map.empty[String, Type]
            if expectedType != Type.Unknown then
              collectTypeParamBindings(ctor.resultType, expectedType, ctor.typeParams.toSet, typeParamBindings)
            val instantiatedResultType = instantiateType(ctor.resultType, typeParamBindings.toMap)
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
            val typeParamBindings = scala.collection.mutable.Map.empty[String, Type]
            if expectedType != Type.Unknown then
              collectTypeParamBindings(resultType, expectedType, ctor.typeParams.toSet, typeParamBindings)
            val subst = typeParamBindings.toMap
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
      bindings.foreach { case (name, symbol) =>
        if acc.contains(name) then
          errors += errorAt(source, s"Duplicate pattern binder: $name")
      }
      acc ++ bindings
    }

  private def collectTypeParamBindings(
      pattern: Type,
      actual: Type,
      typeParams: Set[String],
      bindings: scala.collection.mutable.Map[String, Type]
    ): Unit =
    (pattern, actual) match
      case (Type.Name(name), _) if typeParams.contains(name) =>
        bindings.get(name) match
          case Some(existing) =>
            if isCompatible(existing, actual) then
              bindings.update(name, unifyTypes(existing, actual))
          case None =>
            bindings.update(name, actual)
      case (Type.App(patternBase, patternArgs), Type.App(actualBase, actualArgs)) =>
        collectTypeParamBindings(patternBase, actualBase, typeParams, bindings)
        patternArgs.zipAll(actualArgs, Type.Unknown, Type.Unknown).foreach { case (patternArg, actualArg) =>
          collectTypeParamBindings(patternArg, actualArg, typeParams, bindings)
        }
      case (Type.Fun(patternParams, patternResult), Type.Fun(actualParams, actualResult)) =>
        patternParams.zipAll(actualParams, Type.Unknown, Type.Unknown).foreach { case (patternParam, actualParam) =>
          collectTypeParamBindings(patternParam, actualParam, typeParams, bindings)
        }
        collectTypeParamBindings(patternResult, actualResult, typeParams, bindings)
      case _ => ()

  private def instantiateType(tpe: Type, bindings: Map[String, Type]): Type =
    tpe match
      case Type.Name(name) => bindings.getOrElse(name, tpe)
      case Type.App(base, args) => Type.App(instantiateType(base, bindings), args.map(instantiateType(_, bindings)))
      case Type.Fun(params, result) =>
        Type.Fun(params.map(instantiateType(_, bindings)), instantiateType(result, bindings))
      case Type.Unknown => Type.Unknown

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
      source: ast.SourceRange
    ): (Type, List[TypeError]) =
    if isCompatible(left, right) then (unifyTypes(left, right), Nil)
    else
      (
        Type.Unknown,
        List(errorAt(source, s"$context branches have incompatible types: ${renderType(left)} vs ${renderType(right)}"))
      )

  private def unifyTypes(left: Type, right: Type): Type =
    (left, right) match
      case (Type.Unknown, other) => other
      case (other, Type.Unknown) => other
      case _ if left == right => left
      case _ => Type.Unknown

  private def ensureExpectedType(
      actualType: Type,
      expectedType: Option[Type],
      source: ast.SourceRange
    ): (Type, List[TypeError]) =
    expectedType match
      case Some(expected) if !isCompatible(expected, actualType) =>
        (
          actualType,
          List(
            errorAt(source, s"Expression has type ${renderType(actualType)}, expected ${renderType(expected)}")
          )
        )
      case Some(expected) =>
        (unifyTypes(expected, actualType), Nil)
      case None =>
        (actualType, Nil)

  private def resolveFunctionType(callee: Expr): Option[(Type.Fun, Set[String])] =
    callee match
      case Expr.Var(symbol, _) =>
        symbol match
          case fun: FunctionSymbol => Some((fun.tpe, fun.typeParams.toSet))
          case builtin: BuiltinFunctionSymbol => Some((builtin.tpe, Set.empty[String]))
          case _ => None
      case _ =>
        callee.tpe match
          case tpe: Type.Fun => Some((tpe, Set.empty[String]))
          case _ => None

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

  private def validateAstType(tpe: ast.Type, typeParams: Set[String], exports: ExportEnv): List[TypeError] =
    tpe match
      case ast.Type.Name(value) =>
        if typeParams.contains(value) || builtinTypeNames.contains(value) || exports.types.contains(value) then Nil
        else List(errorAt(tpe.source, s"Unknown type name: $value"))
      case ast.Type.App(base, args) =>
        validateAstType(base, typeParams, exports) ++ args.flatMap(arg => validateAstType(arg, typeParams, exports))
      case ast.Type.Paren(inner) =>
        validateAstType(inner, typeParams, exports)

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

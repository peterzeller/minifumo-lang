package com.github.peterzeller.minifumo.ast

import com.github.peterzeller.minifumo.antlr.MinifumoParser
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*
import java.util.Arrays

object AstTransform:
  def program(ctx: MinifumoParser.ProgramContext): ProgramFile =
    ProgramFile(
      ctx.importStatement().asScala.toList.map(importStatement),
      ctx.topLevel().asScala.toList.map(topLevel)
    )(range(ctx))

  def importStatement(ctx: MinifumoParser.ImportStatementContext): ImportStatement =
    val name = ctx.ID().getText
    val from = Option(ctx.from).map(token => unquote(token.getText))
    val inRepo = Option(ctx.in).map(token => unquote(token.getText))
    ImportStatement(name, from, inRepo)(range(ctx))

  def topLevel(ctx: MinifumoParser.TopLevelContext): TopLevel =
    if ctx.dataDecl() != null then
      dataDecl(ctx.dataDecl())
    else
      funDecl(ctx.funDecl())

  def dataDecl(ctx: MinifumoParser.DataDeclContext): TopLevel =
    val name = typeName(ctx.typeName())
    val params = Option(ctx.implicitParams()).map(typeParams).getOrElse(Nil)
    val ctors = ctx.ctorDecl().asScala.toList.map(ctorDecl)
    TopLevel.DataDecl(name, params, ctors, isExported(ctx.EXPORT()))(range(ctx))

  def typeName(ctx: MinifumoParser.TypeNameContext): String =
    ctx.ID().getText

  def typeParams(ctx: MinifumoParser.ImplicitParamsContext): List[FunParam] =
    ctx.funParam().asScala.toList.map(funParam)


  def ctorDecl(ctx: MinifumoParser.CtorDeclContext): CtorDecl =
    val name = ctx.ID().getText
    val fields = Option(ctx.ctorFields()).map(ctorFields).getOrElse(Nil)
    CtorDecl(name, fields)(range(ctx))

  def ctorFields(ctx: MinifumoParser.CtorFieldsContext): List[CtorField] =
    ctx.ctorField().asScala.toList.map(ctorField)

  def ctorField(ctx: MinifumoParser.CtorFieldContext): CtorField =
    val fieldType = Option(ctx.expr()).map(expr).getOrElse(Expr.Hole()(range(ctx)))
    CtorField(ctx.ID().getText, fieldType)(range(ctx))

  def funDecl(ctx: MinifumoParser.FunDeclContext): TopLevel =
    val sig = ctx.funSig()
    val funSigDecl = funSig(sig)
    val body = suite(ctx.suite())
    TopLevel.FunDecl(
      funSigDecl,
      body,
      isExported(ctx.EXPORT())
    )(range(ctx))

  def funSig(ctx: MinifumoParser.FunSigContext): FunSig =
    val name = ctx.ID().getText
    val params = Option(ctx.funParams()).map(funParams).getOrElse(Nil)
    val tParams = Option(ctx.implicitParams()).map(implicitParams).getOrElse(Nil)
    val returnType = expr(ctx.expr())
    FunSig(name, tParams, params, returnType)(range(ctx))

  def implicitParams(ctx: MinifumoParser.ImplicitParamsContext): List[FunParam] =
    ctx.funParam().asScala.toList.map(funParam)

  def funParams(ctx: MinifumoParser.FunParamsContext): List[FunParam] =
    ctx.funParam().asScala.toList.map(funParam)

  def funParam(ctx: MinifumoParser.FunParamContext): FunParam =
    val paramType = Option(ctx.expr()).map(expr).getOrElse(Expr.Hole()(range(ctx)))
    FunParam(ctx.ID().getText, paramType)(range(ctx))

  // Parses a suite and desugars it into a single expression.
  def suite(ctx: MinifumoParser.SuiteContext): Expr =
    desugarBlock(block(ctx.block()), range(ctx))

  // Parses a block into a list of expressions.
  def block(ctx: MinifumoParser.BlockContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def expr(ctx: MinifumoParser.ExprContext): Expr =
    ctx match
      case c: MinifumoParser.LitContext =>
        Expr.Lit(literal(c.literal()))(range(c))
      case c: MinifumoParser.VarContext =>
        Expr.Var(c.ID().getText)(range(c))
      case c: MinifumoParser.ParenContext =>
        expr(c.expr())
      case c: MinifumoParser.LambdaContext =>
        val (params, bodyExpr) = lambdaExpr(c)
        lambdaChain(params, bodyExpr, range(c))
      case c: MinifumoParser.CallContext =>
        val args = Option(c.argList()).map(argList).getOrElse(Nil)
        val tArgs = Option(c.typeArgs()).map(typeArgs).getOrElse(Nil)
        curriedCall(expr(c.expr()), tArgs, args, range(c))
      case c: MinifumoParser.DotContext =>
        // The expression x.f(a,b,c) is short for f(x,a,b,c)
        curriedCall(Expr.Var(c.ID().getText)(range(c)), Nil, expr(c.expr()) :: argList(c.argList()), range(c))
      case c: MinifumoParser.NegContext =>
        opCall("opNeg", List(expr(c.expr())), range(c))
      case c: MinifumoParser.MulDivContext =>
        opCall(binaryOpName(c.op.getText), List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.AddSubContext =>
        opCall(binaryOpName(c.op.getText), List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.CompareContext =>
        val left = expr(c.expr(0))
        val right = expr(c.expr(1))
        val op = c.op.getText
        if op == ">" then
          opCall("opLt", List(right, left), range(c))
        else if op == ">=" then
          opCall("opLe", List(right, left), range(c))
        else
          opCall(binaryOpName(op), List(left, right), range(c))
      case c: MinifumoParser.EqNeqContext =>
        val left = expr(c.expr(0))
        val right = expr(c.expr(1))
        if c.op.getText == "!=" then
          val eqExpr = opCall("eq", List(left, right), range(c))
          makeCall(Expr.Var("opNot")(range(c)), Nil, List(eqExpr))(range(c))
        else
          opCall("eq", List(left, right), range(c))
      case c: MinifumoParser.AndContext =>
        opCall("opAnd", List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.OrContext =>
        opCall("opOr", List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.FunctionTypeContext =>
        val baseExpr = expr(c.base)
        val resultExpr = expr(c.result)
        val (baseParams, baseResult) = splitFunctionType(baseExpr)
        val allParams = baseParams :+ baseResult
        allParams.foldRight(resultExpr) { (paramType, acc) =>
          val param = PiParam("_", paramType)(range(c))
          Expr.Pi(param, acc)(range(c))
        }
      case c: MinifumoParser.DependentFunctionTypeContext =>
        val param = PiParam(c.ID().getText, expr(c.base))(range(c))
        Expr.Pi(param, expr(c.result))(range(c))
      case c: MinifumoParser.LetInContext =>
        val tpe = Option(c.varType).map(expr)
        Expr.LetIn(c.ID().getText, tpe, expr(c.expr(0)), expr(c.expr(1)))(range(c))
      case c: MinifumoParser.LetStmtContext =>
        val tpe = Option(c.varType).map(expr)
        Expr.LetIn(c.ID().getText, tpe, expr(c.value), Expr.Hole()(range(c)))(range(c))
      case c: MinifumoParser.IfThenElseContext =>
        boolMatch(expr(c.expr(0)), expr(c.expr(1)), expr(c.expr(2)), range(c))
      case c: MinifumoParser.IfSuiteContext =>
        val thenExpr = suite(c.suite(0))
        val elseExpr = suite(c.suite(1))
        boolMatch(expr(c.expr()), thenExpr, elseExpr, range(c))
      case c: MinifumoParser.MatchContext =>
        Expr.Match(expr(c.expr()), c.matchCase().asScala.toList.map(matchCase))(range(c))
      case c: MinifumoParser.UnitContext =>
        Expr.Lit(Literal.UnitLit()(range(c)))(range(c))
      case other =>
        throw new IllegalArgumentException(s"Unexpected expr context: ${other.getClass.getSimpleName} at ${range(other)} // ${other.getText()} // ${other.toStringTree(Arrays.asList(MinifumoParser.ruleNames*))}")

  def makeCall(callee: Expr, implicitArgs: List[Expr], args: List[Expr])(source: SourceRange): Expr =
    curriedCall(callee, implicitArgs, args, source)

  def argList(ctx: MinifumoParser.ArgListContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def typeArgs(ctx: MinifumoParser.TypeArgsContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def matchCase(ctx: MinifumoParser.MatchCaseContext): MatchCase =
    MatchCase(pattern(ctx.pattern()), suite(ctx.suite()))(range(ctx))

  def literal(ctx: MinifumoParser.LiteralContext): Literal =
    if ctx.INT() != null then
      Literal.IntLit(ctx.INT().getText)(range(ctx))
    else if ctx.BOOL() != null then
      Literal.BoolLit(ctx.BOOL().getText == "true")(range(ctx))
    else
      Literal.StringLit(unquote(ctx.STRING().getText))(range(ctx))

  def pattern(ctx: MinifumoParser.PatternContext): Pattern =
    ctx match
      case _: MinifumoParser.PatWildcardContext =>
        Pattern.Wildcard()(range(ctx))
      case c: MinifumoParser.PatLitContext =>
        Pattern.Lit(literal(c.literal()))(range(c))
      case c: MinifumoParser.PatBinderOrCtor0Context =>
        Pattern.BinderOrCtor0(c.ID().getText)(range(c))
      case c: MinifumoParser.PatCtorContext =>
        val args = Option(c.patternArgs()).map(patternArgs).getOrElse(Nil)
        Pattern.Ctor(c.ID().getText, args)(range(c))
      case c: MinifumoParser.PatParenContext =>
        pattern(c.pattern())
      case other =>
        throw new IllegalArgumentException(s"Unexpected pattern context: ${other.getClass.getSimpleName}")

  def patternArgs(ctx: MinifumoParser.PatternArgsContext): List[Pattern] =
    ctx.pattern().asScala.toList.map(pattern)

  // Builds a curried call expression from explicit arguments.
  private def curriedCall(
      callee: Expr,
      typeArgs: List[Expr],
      args: List[Expr],
      source: SourceRange
    ): Expr =
      (typeArgs, args) match
        case (Nil, Nil) => callee
        case (tArg :: tTail, _) =>
          curriedCall(Expr.CallImplicit(callee, tArg)(callee.source.merge(tArg.source)), tTail, args, source)
        case (_, arg :: tail) =>
          curriedCall(Expr.Call(callee, arg)(callee.source.merge(arg.source)), Nil, tail, source)

  // Desugars an if-then-else expression into a Bool match.
  private def boolMatch(cond: Expr, thenExpr: Expr, elseExpr: Expr, source: SourceRange): Expr =
    val trueCase = MatchCase(Pattern.BinderOrCtor0("True")(source), thenExpr)(source)
    val falseCase = MatchCase(Pattern.BinderOrCtor0("False")(source), elseExpr)(source)
    Expr.Match(cond, List(trueCase, falseCase))(source)

  // Parses a lambda expression context into parameters and a body expression.
  private def lambdaExpr(ctx: MinifumoParser.LambdaContext): (List[LambdaParam], Expr) =
    val params = lambdaParams(ctx.lambdaParams())
    val bodyExpr = expr(ctx.expr())
    (params, bodyExpr)

  // Parses lambda parameters with optional types.
  private def lambdaParams(ctx: MinifumoParser.LambdaParamsContext): List[LambdaParam] =
    ctx match
      case single: MinifumoParser.LambdaSingleContext =>
        Option(single.lambdaParam()).toList.map(lambdaParam)
      case multi: MinifumoParser.LambdaMultiContext =>
        multi.lambdaParam().asScala.toList.map(lambdaParam)
      case other =>
        throw new IllegalArgumentException(s"Unexpected lambda params context: ${other.getClass.getSimpleName}")

  // Parses a single lambda parameter with optional type.
  private def lambdaParam(ctx: MinifumoParser.LambdaParamContext): LambdaParam =
    val tpe = Option(ctx.expr()).map(expr)
    LambdaParam(ctx.ID().getText, tpe)(range(ctx))

  // Splits a non-dependent function type into parameter types and the final result type.
  private def splitFunctionType(expr: Expr): (List[Expr], Expr) =
    expr match
      case Expr.Pi(param, body) =>
        val (params, result) = splitFunctionType(body)
        (param.tpe :: params, result)
      case _ => (Nil, expr)

  // Builds a nested lambda chain from multiple parameters.
  private def lambdaChain(params: List[LambdaParam], body: Expr, source: SourceRange): Expr =
    params match
      case head :: tail =>
        Expr.Lambda(head, lambdaChain(tail, body, source))(source)
      case Nil =>
        body

  // Desugars a block of expressions into nested let-bindings.
  private def desugarBlock(exprs: List[Expr], source: SourceRange): Expr =
    val unitLiteral = Literal.UnitLit()(source)
    val unitExpr = Expr.Lit(unitLiteral)(source)
    exprs match
      case Nil =>
        unitExpr
      case List(e) => e
      case head :: tail =>
        head match
          case Expr.LetIn(name, tpe, value, Expr.Hole()) =>
            Expr.LetIn(name, tpe, value, desugarBlock(tail, source))(head.source)
          case other =>
              Expr.LetIn("_", None, other, desugarBlock(tail, source))(head.source)

  private def unquote(text: String): String =
    if text.length >= 2 && text.head == '"' && text.last == '"' then
      text.substring(1, text.length - 1)
    else
      text

  private def isExported(token: TerminalNode | Null): Boolean =
    token != null

  private def opCall(name: String, args: List[Expr], source: SourceRange): Expr =
    curriedCall(Expr.Var(name)(source), Nil, args, source)

  // Maps operator symbols to standard library operator function names.
  private def binaryOpName(symbol: String): String =
    symbol match
      case "+" => "opPlus"
      case "-" => "opMinus"
      case "*" => "opTimes"
      case "/" => "opDiv"
      case "%" => "opMod"
      case "<" => "opLt"
      case "<=" => "opLe"
      case other => other

  private def range(ctx: ParserRuleContext): SourceRange =
    rangeFromTokens(ctx.getStart, ctx.getStop)

  private def rangeFromTokens(start: Token, stop: Token): SourceRange =
    val startToken = Option(start).getOrElse(stop)
    val endToken = Option(stop).getOrElse(start)
    val startPos = SourcePos(startToken.getLine, startToken.getCharPositionInLine + 1)
    val endPos = SourcePos(endToken.getLine, endToken.getCharPositionInLine + tokenTextLength(endToken) + 1)
    SourceRange(startPos, endPos)



  private def tokenTextLength(token: Token): Int =
    Option(token.getText).fold(0)(_.length)

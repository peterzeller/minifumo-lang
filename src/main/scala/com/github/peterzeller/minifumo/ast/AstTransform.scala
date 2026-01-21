package com.github.peterzeller.minifumo.ast

import com.github.peterzeller.minifumo.antlr.MinifumoParser

import scala.jdk.CollectionConverters.*

object AstTransform:
  def program(ctx: MinifumoParser.ProgramContext): ProgramFile =
    ProgramFile(ctx.topLevel().asScala.toList.map(topLevel))

  def topLevel(ctx: MinifumoParser.TopLevelContext): TopLevel =
    if ctx.dataDecl() != null then
      dataDecl(ctx.dataDecl())
    else
      funDecl(ctx.funDecl())

  def dataDecl(ctx: MinifumoParser.DataDeclContext): TopLevel =
    val name = typeName(ctx.typeName())
    val params = Option(ctx.typeParams()).map(typeParams).getOrElse(Nil)
    val ctors = ctx.ctorDecl().asScala.toList.map(ctorDecl)
    TopLevel.DataDecl(name, params, ctors)

  def typeName(ctx: MinifumoParser.TypeNameContext): String =
    ctx.ID().getText

  def typeParams(ctx: MinifumoParser.TypeParamsContext): List[String] =
    ctx.ID().asScala.toList.map(_.getText)

  def ctorDecl(ctx: MinifumoParser.CtorDeclContext): CtorDecl =
    val name = ctx.ID().getText
    val fields = Option(ctx.ctorFields()).map(ctorFields).getOrElse(Nil)
    CtorDecl(name, fields)

  def ctorFields(ctx: MinifumoParser.CtorFieldsContext): List[CtorField] =
    ctx.ctorField().asScala.toList.map(ctorField)

  def ctorField(ctx: MinifumoParser.CtorFieldContext): CtorField =
    CtorField(ctx.ID().getText, `type`(ctx.`type`()))

  def funDecl(ctx: MinifumoParser.FunDeclContext): TopLevel =
    val sig = ctx.funSig()
    val name = sig.ID().getText
    val params = Option(sig.funParams()).map(funParams).getOrElse(Nil)
    val tParams = Option(sig.typeParams()).map(typeParams).getOrElse(Nil)
    val returnType = Option(sig.`type`()).map(`type`)
    val body = suite(ctx.suite())
    TopLevel.FunDecl(name, tParams, params, returnType, body)

  def funParams(ctx: MinifumoParser.FunParamsContext): List[FunParam] =
    ctx.funParam().asScala.toList.map(funParam)

  def funParam(ctx: MinifumoParser.FunParamContext): FunParam =
    FunParam(ctx.ID().getText, `type`(ctx.`type`()))

  def suite(ctx: MinifumoParser.SuiteContext): Suite =
    if ctx.block() != null then
      Suite.Block(block(ctx.block()))
    else
      Suite.Single(expr(ctx.expr()))

  def block(ctx: MinifumoParser.BlockContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def `type`(ctx: MinifumoParser.TypeContext): Type =
    val base = typeAtom(ctx.typeAtom())
    ctx.typeApp().asScala.toList.map(typeApp).foldLeft(base) { (acc, args) =>
      Type.App(acc, args)
    }

  def typeAtom(ctx: MinifumoParser.TypeAtomContext): Type =
    if ctx.ID() != null then
      Type.Name(ctx.ID().getText)
    else
      Type.Paren(`type`(ctx.`type`()))

  def typeApp(ctx: MinifumoParser.TypeAppContext): List[Type] =
    ctx.`type`().asScala.toList.map(`type`)

  def expr(ctx: MinifumoParser.ExprContext): Expr =
    ctx match
      case c: MinifumoParser.LitContext =>
        Expr.Lit(literal(c.literal()))
      case c: MinifumoParser.VarContext =>
        Expr.Var(c.ID().getText)
      case c: MinifumoParser.ParenContext =>
        Expr.Paren(expr(c.expr()))
      case c: MinifumoParser.CallContext =>
        val args = Option(c.argList()).map(argList).getOrElse(Nil)
        Expr.Call(expr(c.expr()), args)
      case c: MinifumoParser.DotContext =>
        // The expression x.f(a,b,c) is short for f(x,a,b,c)
        opCall(c.ID().getText(), expr(c.expr()) :: argList(c.argList()))
      case c: MinifumoParser.NegContext =>
        opCall("-", List(expr(c.expr())))
      case c: MinifumoParser.MulDivContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))))
      case c: MinifumoParser.AddSubContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))))
      case c: MinifumoParser.CompareContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))))
      case c: MinifumoParser.EqNeqContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))))
      case c: MinifumoParser.AndContext =>
        opCall("and", List(expr(c.expr(0)), expr(c.expr(1))))
      case c: MinifumoParser.OrContext =>
        opCall("or", List(expr(c.expr(0)), expr(c.expr(1))))
      case c: MinifumoParser.LetInContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, true, tpe, expr(c.expr(0)), expr(c.expr(1)))
      case c: MinifumoParser.VarInContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, false, tpe, expr(c.expr(0)), expr(c.expr(1)))
      case c: MinifumoParser.LetSuiteContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, true, tpe, expr(c.expr()), suiteToExpr(suite(c.suite())))
      case c: MinifumoParser.LetSuiteNoInitContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, true, tpe, uninitializedValue, suiteToExpr(suite(c.suite())))
      case c: MinifumoParser.VarSuiteContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, false, tpe, expr(c.expr()), suiteToExpr(suite(c.suite())))
      case c: MinifumoParser.VarSuiteNoInitContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, false, tpe, uninitializedValue, suiteToExpr(suite(c.suite())))
      case c: MinifumoParser.BindSuiteContext =>
        Expr.LetIn(c.ID().getText, false, None, expr(c.expr()), suiteToExpr(suite(c.suite())))
      case c: MinifumoParser.IfThenElseContext =>
        Expr.IfThenElse(expr(c.expr(0)), expr(c.expr(1)), expr(c.expr(2)))
      case c: MinifumoParser.IfSuiteContext =>
        val suites = c.suite().asScala.toList.map(suite)
        val thenExpr = suiteToExpr(suites.head)
        val elseExpr = suites.drop(1).headOption.map(suiteToExpr).getOrElse(Expr.Var("unit"))
        Expr.IfThenElse(expr(c.expr()), thenExpr, elseExpr)
      case c: MinifumoParser.ForContext =>
        Expr.For(c.ID().getText, expr(c.expr()), suite(c.suite()))
      case c: MinifumoParser.WhileContext =>
        Expr.While(expr(c.expr()), suite(c.suite()))
      case c: MinifumoParser.MatchContext =>
        Expr.Match(expr(c.expr()), c.matchCase().asScala.toList.map(matchCase))
      case other =>
        throw new IllegalArgumentException(s"Unexpected expr context: ${other.getClass.getSimpleName}")

  def argList(ctx: MinifumoParser.ArgListContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def matchCase(ctx: MinifumoParser.MatchCaseContext): MatchCase =
    MatchCase(pattern(ctx.pattern()), suite(ctx.suite()))

  def literal(ctx: MinifumoParser.LiteralContext): Literal =
    if ctx.INT() != null then
      Literal.IntLit(ctx.INT().getText)
    else if ctx.BOOL() != null then
      Literal.BoolLit(ctx.BOOL().getText == "true")
    else
      Literal.StringLit(unquote(ctx.STRING().getText))

  def pattern(ctx: MinifumoParser.PatternContext): Pattern =
    ctx match
      case _: MinifumoParser.PatWildcardContext =>
        Pattern.Wildcard
      case c: MinifumoParser.PatLitContext =>
        Pattern.Lit(literal(c.literal()))
      case c: MinifumoParser.PatBinderOrCtor0Context =>
        Pattern.BinderOrCtor0(c.ID().getText)
      case c: MinifumoParser.PatCtorContext =>
        val args = Option(c.patternArgs()).map(patternArgs).getOrElse(Nil)
        Pattern.Ctor(c.ID().getText, args)
      case c: MinifumoParser.PatParenContext =>
        pattern(c.pattern())
      case other =>
        throw new IllegalArgumentException(s"Unexpected pattern context: ${other.getClass.getSimpleName}")

  def patternArgs(ctx: MinifumoParser.PatternArgsContext): List[Pattern] =
    ctx.pattern().asScala.toList.map(pattern)

  private def unquote(text: String): String =
    if text.length >= 2 && text.head == '"' && text.last == '"' then
      text.substring(1, text.length - 1)
    else
      text

  private def opCall(name: String, args: List[Expr]): Expr =
    Expr.Call(Expr.Var(name), args)

  private def suiteToExpr(s: Suite): Expr =
    s match
      case Suite.Single(e) => e
      case Suite.Block(exprs) => opCall("block", exprs)

  private def uninitializedValue: Expr =
    Expr.Var("undefined")

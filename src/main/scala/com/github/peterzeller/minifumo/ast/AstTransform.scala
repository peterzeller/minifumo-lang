package com.github.peterzeller.minifumo.ast

import com.github.peterzeller.minifumo.antlr.MinifumoParser
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*

object AstTransform:
  def program(ctx: MinifumoParser.ProgramContext): ProgramFile =
    ProgramFile(ctx.topLevel().asScala.toList.map(topLevel))(range(ctx))

  def topLevel(ctx: MinifumoParser.TopLevelContext): TopLevel =
    if ctx.dataDecl() != null then
      dataDecl(ctx.dataDecl())
    else if ctx.typeClassDecl() != null then
      typeClassDecl(ctx.typeClassDecl())
    else if ctx.typeClassImpl() != null then
      typeClassImpl(ctx.typeClassImpl())
    else
      funDecl(ctx.funDecl())

  def dataDecl(ctx: MinifumoParser.DataDeclContext): TopLevel =
    val name = typeName(ctx.typeName())
    val params = Option(ctx.typeParams()).map(typeParams).getOrElse(Nil)
    val ctors = ctx.ctorDecl().asScala.toList.map(ctorDecl)
    TopLevel.DataDecl(name, params, ctors)(range(ctx))

  def typeName(ctx: MinifumoParser.TypeNameContext): String =
    ctx.ID().getText

  def typeParams(ctx: MinifumoParser.TypeParamsContext): List[String] =
    ctx.ID().asScala.toList.map(_.getText)

  def ctorDecl(ctx: MinifumoParser.CtorDeclContext): CtorDecl =
    val name = ctx.ID().getText
    val fields = Option(ctx.ctorFields()).map(ctorFields).getOrElse(Nil)
    CtorDecl(name, fields)(range(ctx))

  def ctorFields(ctx: MinifumoParser.CtorFieldsContext): List[CtorField] =
    ctx.ctorField().asScala.toList.map(ctorField)

  def ctorField(ctx: MinifumoParser.CtorFieldContext): CtorField =
    CtorField(ctx.ID().getText, `type`(ctx.`type`()))(range(ctx))

  def funDecl(ctx: MinifumoParser.FunDeclContext): TopLevel =
    val sig = ctx.funSig()
    val funSigDecl = funSig(sig)
    val body = suite(ctx.suite())
    TopLevel.FunDecl(
      funSigDecl.name,
      funSigDecl.typeParams,
      funSigDecl.params,
      funSigDecl.returnType,
      funSigDecl.givenParams,
      body
    )(range(ctx))

  def funSig(ctx: MinifumoParser.FunSigContext): FunSig =
    val name = ctx.ID().getText
    val params = Option(ctx.funParams()).map(funParams).getOrElse(Nil)
    val tParams = Option(ctx.typeParams()).map(typeParams).getOrElse(Nil)
    val returnType = Option(ctx.`type`()).map(`type`)
    val givenParams = Option(ctx.givenClause()).map(givenClause).getOrElse(Nil)
    FunSig(name, tParams, params, returnType, givenParams)(range(ctx))

  def typeClassDecl(ctx: MinifumoParser.TypeClassDeclContext): TopLevel =
    val name = ctx.ID().getText
    val tParams = Option(ctx.typeParams()).map(typeParams).getOrElse(Nil)
    val members = Option(ctx.typeClassSigBlock()).map(typeClassSigBlock).getOrElse(Nil)
    TopLevel.TypeClassDecl(name, tParams, members)(range(ctx))

  def typeClassImpl(ctx: MinifumoParser.TypeClassImplContext): TopLevel =
    val name = ctx.name.getText
    val tParams = Option(ctx.typeParams()).map(typeParams).getOrElse(Nil)
    val typeClassName = ctx.ID().get(1).getText
    val head = typeClassHead(typeClassName, Option(ctx.typeArgs()).map(typeArgs).getOrElse(Nil), range(ctx))
    val givenParams = Option(ctx.givenClause()).map(givenClause).getOrElse(Nil)
    val members = Option(ctx.typeClassImplBlock()).map(typeClassImplBlock).getOrElse(Nil)
    TopLevel.InstanceDecl(name, tParams, head, givenParams, members)(range(ctx))

  def funParams(ctx: MinifumoParser.FunParamsContext): List[FunParam] =
    ctx.funParam().asScala.toList.map(funParam)

  def funParam(ctx: MinifumoParser.FunParamContext): FunParam =
    FunParam(ctx.ID().getText, `type`(ctx.`type`()))(range(ctx))

  def givenClause(ctx: MinifumoParser.GivenClauseContext): List[FunParam] =
    funParams(ctx.funParams())

  def suite(ctx: MinifumoParser.SuiteContext): Suite =
    Suite.Block(block(ctx.block()))(range(ctx))

  def block(ctx: MinifumoParser.BlockContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def `type`(ctx: MinifumoParser.TypeContext): Type =
    val base = typeAtom(ctx.typeAtom())
    ctx.typeApp().asScala.toList.map(typeApp).foldLeft(base) { (acc, args) =>
      Type.App(acc, args)(range(ctx))
    }

  def typeAtom(ctx: MinifumoParser.TypeAtomContext): Type =
    if ctx.ID() != null then
      Type.Name(ctx.ID().getText)(range(ctx))
    else
      Type.Paren(`type`(ctx.`type`()))(range(ctx))

  def typeApp(ctx: MinifumoParser.TypeAppContext): List[Type] =
    ctx.`type`().asScala.toList.map(`type`)

  def expr(ctx: MinifumoParser.ExprContext): Expr =
    ctx match
      case c: MinifumoParser.LitContext =>
        Expr.Lit(literal(c.literal()))(range(c))
      case c: MinifumoParser.VarContext =>
        Expr.Var(c.ID().getText)(range(c))
      case c: MinifumoParser.ParenContext =>
        Expr.Paren(expr(c.expr()))(range(c))
      case c: MinifumoParser.CallContext =>
        val args = Option(c.argList()).map(argList).getOrElse(Nil)
        val tArgs = Option(c.typeArgs()).map(typeArgs).getOrElse(Nil)
        val usingArgs = Option(c.usingClause()).map(usingClause).getOrElse(Nil)
        Expr.Call(expr(c.expr()), tArgs, args, usingArgs)(range(c))
      case c: MinifumoParser.DotContext =>
        // The expression x.f(a,b,c) is short for f(x,a,b,c)
        opCall(c.ID().getText(), expr(c.expr()) :: argList(c.argList()), range(c))
      case c: MinifumoParser.NegContext =>
        opCall("-", List(expr(c.expr())), range(c))
      case c: MinifumoParser.MulDivContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.AddSubContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.CompareContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.EqNeqContext =>
        opCall(c.op.getText, List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.AndContext =>
        opCall("and", List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.OrContext =>
        opCall("or", List(expr(c.expr(0)), expr(c.expr(1))), range(c))
      case c: MinifumoParser.LetInContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, true, tpe, expr(c.expr(0)), expr(c.expr(1)))(range(c))
      case c: MinifumoParser.VarInContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.LetIn(c.ID().getText, false, tpe, expr(c.expr(0)), expr(c.expr(1)))(range(c))
      case c: MinifumoParser.LetStmtContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.Bind(c.ID().getText, true, tpe, expr(c.expr()))(range(c))
      case c: MinifumoParser.LetStmtNoInitContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.Bind(c.ID().getText, true, tpe, uninitializedValue(range(c)))(range(c))
      case c: MinifumoParser.VarStmtContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.Bind(c.ID().getText, false, tpe, expr(c.expr()))(range(c))
      case c: MinifumoParser.VarStmtNoInitContext =>
        val tpe = Option(c.`type`()).map(`type`)
        Expr.Bind(c.ID().getText, false, tpe, uninitializedValue(range(c)))(range(c))
      case c: MinifumoParser.AssignContext =>
        Expr.Assign(c.ID().getText, expr(c.expr()))(range(c))
      case c: MinifumoParser.IfThenElseContext =>
        Expr.IfThenElse(expr(c.expr(0)), expr(c.expr(1)), expr(c.expr(2)))(range(c))
      case c: MinifumoParser.IfSuiteContext =>
        val suites = c.suite().asScala.toList.map(suite)
        val thenExpr = suiteToExpr(suites.head)
        val elseExpr = suites.drop(1).headOption.map(suiteToExpr).getOrElse(Expr.Var("unit")(range(c)))
        Expr.IfThenElse(expr(c.expr()), thenExpr, elseExpr)(range(c))
      case c: MinifumoParser.ForContext =>
        Expr.For(c.ID().getText, expr(c.expr()), suite(c.suite()))(range(c))
      case c: MinifumoParser.WhileContext =>
        Expr.While(expr(c.expr()), suite(c.suite()))(range(c))
      case c: MinifumoParser.MatchContext =>
        Expr.Match(expr(c.expr()), c.matchCase().asScala.toList.map(matchCase))(range(c))
      case other =>
        throw new IllegalArgumentException(s"Unexpected expr context: ${other.getClass.getSimpleName}")

  def argList(ctx: MinifumoParser.ArgListContext): List[Expr] =
    ctx.expr().asScala.toList.map(expr)

  def typeArgs(ctx: MinifumoParser.TypeArgsContext): List[Type] =
    ctx.`type`().asScala.toList.map(`type`)

  def usingClause(ctx: MinifumoParser.UsingClauseContext): List[Expr] =
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

  def typeClassSigBlock(ctx: MinifumoParser.TypeClassSigBlockContext): List[FunSig] =
    ctx.funSig().asScala.toList.map(funSig)

  def typeClassImplBlock(ctx: MinifumoParser.TypeClassImplBlockContext): List[TopLevel.FunDecl] =
    ctx.funDecl().asScala.toList.map { funDecl =>
      val sig = funSig(funDecl.funSig())
      TopLevel.FunDecl(sig.name, sig.typeParams, sig.params, sig.returnType, sig.givenParams, suite(funDecl.suite()))(
        range(funDecl)
      )
    }

  private def typeClassHead(name: String, args: List[Type], source: SourceRange): Type =
    if args.isEmpty then Type.Name(name)(source) else Type.App(Type.Name(name)(source), args)(source)

  private def unquote(text: String): String =
    if text.length >= 2 && text.head == '"' && text.last == '"' then
      text.substring(1, text.length - 1)
    else
      text

  private def opCall(name: String, args: List[Expr], source: SourceRange): Expr =
    Expr.Call(Expr.Var(name)(source), Nil, args, Nil)(source)

  private def suiteToExpr(s: Suite): Expr =
    s match
      case Suite.Single(e) => e
      case Suite.Block(exprs) =>
        exprs match
          case Nil => Expr.Var("unit")(suiteSource(s))
          case _ => Expr.Block(exprs)(suiteSource(s))

  private def uninitializedValue(source: SourceRange): Expr =
    Expr.Var("undefined")(source)

  private def suiteSource(suite: Suite): SourceRange =
    suite.source

  private def range(ctx: ParserRuleContext): SourceRange =
    rangeFromTokens(ctx.getStart, ctx.getStop)

  private def range(node: TerminalNode): SourceRange =
    rangeFromToken(node.getSymbol)

  private def rangeFromTokens(start: Token, stop: Token): SourceRange =
    val startToken = Option(start).getOrElse(stop)
    val endToken = Option(stop).getOrElse(start)
    val startPos = SourcePos(startToken.getLine, startToken.getCharPositionInLine + 1)
    val endPos = SourcePos(endToken.getLine, endToken.getCharPositionInLine + tokenTextLength(endToken) + 1)
    SourceRange(startPos, endPos)

  private def rangeFromToken(token: Token): SourceRange =
    rangeFromTokens(token, token)

  private def tokenTextLength(token: Token): Int =
    Option(token.getText).fold(0)(_.length)

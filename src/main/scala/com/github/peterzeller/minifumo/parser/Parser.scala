package com.github.peterzeller.minifumo.parser

import com.github.peterzeller.minifumo.ast.*
import com.github.peterzeller.minifumo.common.MinifumoError
import com.github.peterzeller.minifumo.lexer.{HandwrittenLexer, Token, TokenKind}

import java.nio.file.{Files, Path}

case class SyntaxError(pos: SourcePos, message: String) extends MinifumoError:
  override def source: SourceRange = SourceRange(pos, pos)

/** Parses source text into a full ProgramFile AST and syntax errors. */
def parseInput(input: String): (ProgramFile, List[SyntaxError]) =
  val stream = HandwrittenLexer.tokens(input).toVector
  val errors = stream.collect { case Left(e) => e }.toList
  val toks = stream.collect { case Right(t) => t }
  val parser = new HandwrittenParser(toks)
  val program = parser.program()
  (program, errors ++ parser.errors)

/** Parses a source file path into a full ProgramFile AST and syntax errors. */
def parseFile(input: Path): (ProgramFile, List[SyntaxError]) =
  parseInput(Files.readString(input))

/** Implements a recursive descent parser with simple synchronization recovery. */
private class HandwrittenParser(tokens: Vector[Token]):
  val errors = scala.collection.mutable.ListBuffer.empty[SyntaxError]
  private var index = 0

  /** Parses the full program with imports and top-level items. */
  def program(): ProgramFile =
    val imports = scala.collection.mutable.ListBuffer.empty[ImportStatement]
    val items = scala.collection.mutable.ListBuffer.empty[TopLevel]
    while !isAtEnd do
      if check(TokenKind.NL) then
      advance()
      else if check(TokenKind.IMPORT) then imports += parseImport()
      else if check(TokenKind.DATA) || check(TokenKind.EXPORT) || check(TokenKind.FUN) then
        parseTopLevel() match
          case Some(t) => items += t
          case None => synchronizeTopLevel()
      else
        error(current, "Expected import, data, or fun declaration.")
        synchronizeTopLevel()
    val src = SourceRange(SourcePos(1, 1), previous.source.end)
    ProgramFile(imports.toList, items.toList)(src)

  /** Parses one import statement line. */
  private def parseImport(): ImportStatement =
    val start = consume(TokenKind.IMPORT, "Expected 'import'.")
    val name = consume(TokenKind.ID, "Expected imported symbol name.")
    var from: Option[String] = None
    var inRepo: Option[String] = None
    if matchKind(TokenKind.FROM) then
      from = Some(unquote(consume(TokenKind.STRING, "Expected import source string.").text))
      if matchKind(TokenKind.IN) then
        inRepo = Some(unquote(consume(TokenKind.STRING, "Expected repository string.").text))
    consumeOptionalNl()
    ImportStatement(name.text, from, inRepo)(merge(start, previous))

  /** Parses one top-level declaration. */
  private def parseTopLevel(): Option[TopLevel] =
    val exported = matchKind(TokenKind.EXPORT)
    if matchKind(TokenKind.DATA) then Some(parseDataDecl(exported, previous))
    else if matchKind(TokenKind.FUN) then Some(parseFunDecl(exported, previous))
    else
      error(current, "Expected data or fun declaration.")
      None

  /** Parses a data declaration and constructors. */
  private def parseDataDecl(exported: Boolean, start: Token): TopLevel =
    val name = consume(TokenKind.ID, "Expected type name.")
    val tParams = parseImplicitParamsFun()
    consume(TokenKind.EQ, "Expected '=' in data declaration.")
    val ctors = scala.collection.mutable.ListBuffer.empty[CtorDecl]
    if check(TokenKind.ID) then
      ctors += parseCtorDecl()
      while matchKind(TokenKind.BAR) do ctors += parseCtorDecl()
    TopLevel.DataDecl(name.text, tParams, ctors.toList, exported)(merge(start, previous))

  /** Parses one constructor declaration with optional fields. */
  private def parseCtorDecl(): CtorDecl =
    val name = consume(TokenKind.ID, "Expected constructor name.")
    val fields =
      if matchKind(TokenKind.PAREN_LEFT) then
        val fs = commaSeparated(TokenKind.PAREN_RIGHT)(parseCtorField)
        consume(TokenKind.PAREN_RIGHT, "Expected ')' after constructor fields.")
        fs
      else Nil
    CtorDecl(name.text, fields)(merge(name, previous))

  /** Parses one constructor field declaration. */
  private def parseCtorField(): CtorField =
    val name = consume(TokenKind.ID, "Expected field name.")
    consume(TokenKind.COLON, "Expected ':' in constructor field.")
    val tpe = parseExpr()
    CtorField(name.text, tpe)(merge(name, tpe.source))

  /** Parses a function declaration with signature and suite body. */
  private def parseFunDecl(exported: Boolean, start: Token): TopLevel =
    val sig = parseFunSig(start)
    val body = parseSuite()
    TopLevel.FunDecl(sig, body, exported)(merge(start, body.source))

  /** Parses a function signature after the leading fun token. */
  private def parseFunSig(start: Token): FunSig =
    val name = consume(TokenKind.ID, "Expected function name.")
    val implicitParams = parseImplicitParamsFun()
    consume(TokenKind.PAREN_LEFT, "Expected '(' in function signature.")
    val params =
      if !check(TokenKind.PAREN_RIGHT) then commaSeparated(TokenKind.PAREN_RIGHT)(parseFunParam)
      else Nil
    consume(TokenKind.PAREN_RIGHT, "Expected ')' after parameters.")
    consume(TokenKind.COLON, "Expected ':' before return type.")
    val ret = parseExpr()
    FunSig(name.text, implicitParams, params, ret)(merge(start, ret.source))

  /** Parses one function parameter with type annotation. */
  private def parseFunParam(): FunParam =
    val name = consume(TokenKind.ID, "Expected parameter name.")
    consume(TokenKind.COLON, "Expected ':' in parameter.")
    val tpe = parseExpr()
    FunParam(name.text, tpe)(merge(name, tpe.source))

  /** Parses square-bracketed implicit parameters. */
  private def parseImplicitParamsFun(): List[FunParam] =
    if matchKind(TokenKind.BRACKET_LEFT) then
      val params = commaSeparated(TokenKind.BRACKET_RIGHT)(parseFunParam)
      consume(TokenKind.BRACKET_RIGHT, "Expected ']' after implicit parameters.")
      params
    else Nil

  /** Parses an indented suite and desugars it to one expression. */
  private def parseSuite(): Expr =
    consume(TokenKind.NL, "Expected newline before suite.")
    consume(TokenKind.BEGIN, "Expected begin of indented suite.")
    val exprs = scala.collection.mutable.ListBuffer.empty[Expr]
    while !check(TokenKind.END) && !isAtEnd do
      while matchKind(TokenKind.NL) do ()
      if !check(TokenKind.END) then exprs += parseExpr()
      while matchKind(TokenKind.NL) do ()
    consume(TokenKind.END, "Expected end of suite.")
    desugarBlock(exprs.toList)

  /** Parses expressions with precedence climbing from lowest to highest. */
  private def parseExpr(): Expr = parseLetIfMatch()

  /** Parses let/if/match forms with the lowest precedence. */
  private def parseLetIfMatch(): Expr =
    if matchKind(TokenKind.LET) then
      val start = previous
      val name = consume(TokenKind.ID, "Expected variable name after let.")
      val tpe = if matchKind(TokenKind.COLON) then Some(parseExpr()) else None
      consume(TokenKind.EQ, "Expected '=' in let expression.")
      val value = parseExpr()
      if matchKind(TokenKind.IN) then
        val body = parseExpr()
        Expr.LetIn(name.text, tpe, value, body)(merge(start, body.source))
      else
        Expr.LetIn(name.text, tpe, value, Expr.Hole()(value.source))(merge(start, value.source))
    else if matchKind(TokenKind.IF) then parseIfExpr(previous)
    else if matchKind(TokenKind.MATCH) then parseMatchExpr(previous)
    else parseForall()

  /** Parses both if-then-else and suite-based if forms into match desugaring. */
  private def parseIfExpr(start: Token): Expr =
    val cond = parseExpr()
    if matchKind(TokenKind.THEN) then
      val thenExpr = parseExpr()
      consume(TokenKind.ELSE, "Expected else branch.")
      val elseExpr = parseExpr()
      boolMatch(cond, thenExpr, elseExpr, merge(start, elseExpr.source))
    else
      val thenSuite = parseSuite()
      consume(TokenKind.ELSE, "Expected else branch.")
      val elseSuite = parseSuite()
      boolMatch(cond, thenSuite, elseSuite, merge(start, elseSuite.source))

  /** Parses a match expression with one or more cases. */
  private def parseMatchExpr(start: Token): Expr =
    val scrutinee = parseExpr()
    consume(TokenKind.NL, "Expected newline before match cases.")
    consume(TokenKind.BEGIN, "Expected begin for match cases.")
    val cases = scala.collection.mutable.ListBuffer.empty[MatchCase]
    while !check(TokenKind.END) && !isAtEnd do
      consume(TokenKind.CASE, "Expected case branch.")
      val pat = parsePattern()
      val body = parseSuite()
      cases += MatchCase(pat, body)(merge(pat.source, body.source))
      while matchKind(TokenKind.NL) do ()
    consume(TokenKind.END, "Expected end of match block.")
    Expr.Match(scrutinee, cases.toList)(merge(start, previous.source))

  /** Parses forall / function type precedence layer. */
  private def parseForall(): Expr =
    if matchKind(TokenKind.FORALL) then
      val start = previous
      val name = consume(TokenKind.ID, "Expected identifier after forall.")
      consume(TokenKind.COLON, "Expected ':' in forall.")
      val base = parseExpr()
      consume(TokenKind.DOT, "Expected '.' in forall.")
      val result = parseExpr()
      Expr.Pi(PiParam(name.text, base)(merge(name, base.source)), result)(merge(start, result.source))
    else parseOr()

  /** Parses left-associative boolean OR expressions. */
  private def parseOr(): Expr = parseBinaryLeft(parseAnd, Set(TokenKind.OR))

  /** Parses left-associative boolean AND expressions. */
  private def parseAnd(): Expr = parseBinaryLeft(parseEq, Set(TokenKind.AND))

  /** Parses equality and inequality expressions. */
  private def parseEq(): Expr = parseBinaryLeft(parseCompare, Set(TokenKind.EQEQ, TokenKind.NOTEQ))

  /** Parses comparison expressions. */
  private def parseCompare(): Expr = parseBinaryLeft(parseAdd, Set(TokenKind.LT, TokenKind.LE, TokenKind.GT, TokenKind.GE))

  /** Parses additive expressions. */
  private def parseAdd(): Expr = parseBinaryLeft(parseMul, Set(TokenKind.PLUS, TokenKind.MINUS))

  /** Parses multiplicative expressions. */
  private def parseMul(): Expr = parseBinaryLeft(parseUnary, Set(TokenKind.MULT, TokenKind.DIV, TokenKind.MOD))

  /** Parses unary expressions. */
  private def parseUnary(): Expr =
    if matchKind(TokenKind.MINUS) then
      val start = previous
      val expr = parseUnary()
      opCall("opNeg", List(expr), merge(start, expr.source))
    else parseArrow()

  /** Parses right-associative function type arrows. */
  private def parseArrow(): Expr =
    val left = parsePostfix()
    if matchKind(TokenKind.ARROW) then
      val right = parseArrow()
      val param = PiParam("_", left)(left.source)
      Expr.Pi(param, right)(merge(left.source, right.source))
    else left

  /** Parses postfix calls, implicit calls, and dot-call sugar. */
  private def parsePostfix(): Expr =
    var expr = parsePrimary()
    var continue = true
    while continue && !isAtEnd do
      if check(TokenKind.BRACKET_LEFT) then
        advance()
        val tArgs = if !check(TokenKind.BRACKET_RIGHT) then commaSeparated(TokenKind.BRACKET_RIGHT)(parseExpr) else Nil
        consume(TokenKind.BRACKET_RIGHT, "Expected ']'.")
        if matchKind(TokenKind.PAREN_LEFT) then
          val args = if !check(TokenKind.PAREN_RIGHT) then commaSeparated(TokenKind.PAREN_RIGHT)(parseExpr) else Nil
          consume(TokenKind.PAREN_RIGHT, "Expected ')' after call arguments.")
          expr = curriedCall(expr, tArgs, args)
        else
          expr = curriedCall(expr, tArgs, Nil)
      else if matchKind(TokenKind.PAREN_LEFT) then
        val args = if !check(TokenKind.PAREN_RIGHT) then commaSeparated(TokenKind.PAREN_RIGHT)(parseExpr) else Nil
        consume(TokenKind.PAREN_RIGHT, "Expected ')' after call arguments.")
        expr = curriedCall(expr, Nil, args)
      else if matchKind(TokenKind.DOT) then
        val method = consume(TokenKind.ID, "Expected field or method name after '.'.")
        val args =
          if matchKind(TokenKind.PAREN_LEFT) then
            val parsed = if !check(TokenKind.PAREN_RIGHT) then commaSeparated(TokenKind.PAREN_RIGHT)(parseExpr) else Nil
            consume(TokenKind.PAREN_RIGHT, "Expected ')' after method arguments.")
            parsed
          else Nil
        expr = curriedCall(Expr.Var(method.text)(method.source), Nil, expr :: args)
      else continue = false
    expr

  /** Parses primary expressions, literals, vars, and lambdas. */
  private def parsePrimary(): Expr =
    if matchKind(TokenKind.INT) then Expr.Lit(Literal.IntLit(previous.text)(previous.source))(previous.source)
    else if matchKind(TokenKind.BOOL) then Expr.Lit(Literal.BoolLit(previous.text == "true")(previous.source))(previous.source)
    else if matchKind(TokenKind.STRING) then Expr.Lit(Literal.StringLit(unquote(previous.text))(previous.source))(previous.source)
    else if check(TokenKind.PAREN_LEFT) && isParenLambdaStart() then parseLambda(true)
    else if matchKind(TokenKind.ID) then Expr.Var(previous.text)(previous.source)
    else if matchKind(TokenKind.PAREN_LEFT) then
      if matchKind(TokenKind.PAREN_RIGHT) then Expr.Lit(Literal.UnitLit()(previous.source))(previous.source)
      else
        val inner = parseExpr()
        consume(TokenKind.PAREN_RIGHT, "Expected ')' after expression.")
        inner
    else if check(TokenKind.ID) && lookAhead(1).exists(t => t.kind == TokenKind.FAT_ARROW || t.kind == TokenKind.COLON) then parseLambda(false)
    else
      error(current, "Expected expression.")
      val holeSource = if isAtEnd then previous.source else current.source
      Expr.Hole()(holeSource)


  /** Checks whether the current parenthesized form is a lambda parameter list. */
  private def isParenLambdaStart(): Boolean =
    var depth = 0
    var i = index
    while i < tokens.length do
      tokens(i).kind match
        case TokenKind.PAREN_LEFT => depth += 1
        case TokenKind.PAREN_RIGHT =>
          depth -= 1
          if depth == 0 then
            return tokens.lift(i + 1).exists(_.kind == TokenKind.FAT_ARROW)
        case _ => ()
      i += 1
    false

  /** Parses lambda expressions with either single or parenthesized parameter forms. */
  private def parseLambda(parenthesized: Boolean): Expr =
    val params =
      if parenthesized then
        consume(TokenKind.PAREN_LEFT, "Expected '('.")
        val ps = commaSeparated(TokenKind.PAREN_RIGHT)(parseLambdaParam)
        consume(TokenKind.PAREN_RIGHT, "Expected ')' after lambda parameters.")
        ps
      else List(parseLambdaParam())
    consume(TokenKind.FAT_ARROW, "Expected '=>' after lambda parameters.")
    val body = parseExpr()
    params.foldRight(body) { (p, acc) => Expr.Lambda(p, acc)(merge(p.source, acc.source)) }

  /** Parses one lambda parameter with optional type annotation. */
  private def parseLambdaParam(): LambdaParam =
    val name = consume(TokenKind.ID, "Expected lambda parameter name.")
    val tpe = if matchKind(TokenKind.COLON) then Some(parseExpr()) else None
    LambdaParam(name.text, tpe)(merge(name, tpe.map(_.source).getOrElse(name.source)))

  /** Parses one pattern for match cases. */
  private def parsePattern(): Pattern =
    if matchKind(TokenKind.UNDERSCORE) then Pattern.Wildcard()(previous.source)
    else if matchKind(TokenKind.INT) then Pattern.Lit(Literal.IntLit(previous.text)(previous.source))(previous.source)
    else if matchKind(TokenKind.BOOL) then Pattern.Lit(Literal.BoolLit(previous.text == "true")(previous.source))(previous.source)
    else if matchKind(TokenKind.STRING) then Pattern.Lit(Literal.StringLit(unquote(previous.text))(previous.source))(previous.source)
    else if matchKind(TokenKind.ID) then
      val name = previous
      if matchKind(TokenKind.PAREN_LEFT) then
        val args = if !check(TokenKind.PAREN_RIGHT) then commaSeparated(TokenKind.PAREN_RIGHT)(parsePattern) else Nil
        consume(TokenKind.PAREN_RIGHT, "Expected ')' after pattern args.")
        Pattern.Ctor(name.text, args)(merge(name, previous))
      else Pattern.BinderOrCtor0(name.text)(name.source)
    else if matchKind(TokenKind.PAREN_LEFT) then
      val p = parsePattern()
      consume(TokenKind.PAREN_RIGHT, "Expected ')' in pattern.")
      p
    else
      error(current, "Expected pattern.")
      Pattern.Wildcard()(current.source)

  /** Parses a left-associative binary expression layer. */
  private def parseBinaryLeft(next: () => Expr, ops: Set[TokenKind]): Expr =
    var left = next()
    while ops.contains(current.kind) do
      val op = advance()
      val right = next()
      left = makeBinary(left, op, right)
    left

  /** Converts binary operators into standard-library function calls. */
  private def makeBinary(left: Expr, op: Token, right: Expr): Expr =
    val src = merge(left.source, right.source)
    op.kind match
      case TokenKind.NOTEQ =>
        val eqExpr = opCall("eq", List(left, right), src)
        makeCall(Expr.Var("opNot")(src), List(eqExpr), src)
      case TokenKind.GT => opCall("opLt", List(right, left), src)
      case TokenKind.GE => opCall("opLe", List(right, left), src)
      case _ => opCall(binaryOpName(op.text), List(left, right), src)

  /** Builds nested call/call-implicit nodes from type and value arguments. */
  private def curriedCall(callee: Expr, tArgs: List[Expr], args: List[Expr]): Expr =
    val withImplicit = tArgs.foldLeft(callee) { (acc, a) => Expr.CallImplicit(acc, a)(merge(acc.source, a.source)) }
    args.foldLeft(withImplicit) { (acc, a) => Expr.Call(acc, a)(merge(acc.source, a.source)) }

  /** Creates nested lets for a suite block sequence. */
  private def desugarBlock(exprs: List[Expr]): Expr =
    exprs match
      case Nil => Expr.Lit(Literal.UnitLit()(SourceRange.empty))(SourceRange.empty)
      case List(e) => e
      case head :: tail =>
        head match
          case Expr.LetIn(name, tpe, value, Expr.Hole()) =>
            Expr.LetIn(name, tpe, value, desugarBlock(tail))(head.source)
          case other => Expr.LetIn("_", None, other, desugarBlock(tail))(other.source)

  /** Desugars if expressions into boolean pattern matching. */
  private def boolMatch(cond: Expr, thenExpr: Expr, elseExpr: Expr, source: SourceRange): Expr =
    val trueCase = MatchCase(Pattern.BinderOrCtor0("True")(source), thenExpr)(source)
    val falseCase = MatchCase(Pattern.BinderOrCtor0("False")(source), elseExpr)(source)
    Expr.Match(cond, List(trueCase, falseCase))(source)

  /** Builds operator calls by name for transformed infix/unary syntax. */
  private def opCall(name: String, args: List[Expr], source: SourceRange): Expr =
    makeCall(Expr.Var(name)(source), args, source)

  /** Builds nested explicit calls from a function expression and arguments. */
  private def makeCall(fn: Expr, args: List[Expr], source: SourceRange): Expr =
    args.foldLeft(fn) { (acc, arg) => Expr.Call(acc, arg)(merge(acc.source, arg.source)) }

  /** Maps parser operators to their runtime builtin names. */
  private def binaryOpName(symbol: String): String =
    symbol match
      case "+" => "opPlus"
      case "-" => "opMinus"
      case "*" => "opTimes"
      case "/" | "div" => "opDiv"
      case "%" | "mod" => "opMod"
      case "<" => "opLt"
      case "<=" => "opLe"
      case "==" => "eq"
      case "and" => "opAnd"
      case "or" => "opOr"
      case other => other

  /** Parses comma-separated lists until a closing token is encountered. */
  private def commaSeparated[A](endKind: TokenKind)(item: () => A): List[A] =
    val out = scala.collection.mutable.ListBuffer.empty[A]
    if check(endKind) then return out.toList
    out += item()
    while matchKind(TokenKind.COMMA) do out += item()
    out.toList

  /** Consumes a trailing newline when present. */
  private def consumeOptionalNl(): Unit =
    if check(TokenKind.NL) then
      advance()

  /** Synchronizes after an error to likely top-level restart points. */
  private def synchronizeTopLevel(): Unit =
    while !isAtEnd && !Set(TokenKind.NL, TokenKind.END, TokenKind.FUN, TokenKind.DATA, TokenKind.IMPORT).contains(current.kind) do
      advance()
    while matchKind(TokenKind.NL) do ()

  /** Reports a parser error at a token position. */
  private def error(token: Token, message: String): Unit =
    errors += SyntaxError(token.source.start, message)

  /** Consumes one token of the expected kind, with soft recovery on mismatch. */
  private def consume(kind: TokenKind, message: String): Token =
    if check(kind) then advance()
    else
      error(current, message)
      advance()

  /** Returns whether the current token is of the given kind. */
  private def check(kind: TokenKind): Boolean = !isAtEnd && current.kind == kind

  /** Checks and consumes one token kind if it matches. */
  private def matchKind(kind: TokenKind): Boolean =
    if check(kind) then
      advance(); true
    else false

  /** Returns the current token, or EOF if out of bounds. */
  private def current: Token =
    if index >= tokens.length then tokens.lastOption.getOrElse(Token.eof(SourcePos(0, 0))) else tokens(index)

  /** Returns the previous token, or current if at stream start. */
  private def previous: Token =
    if index <= 0 then current else tokens(index - 1)

  /** Peeks ahead by offset without consuming. */
  private def lookAhead(offset: Int): Option[Token] = tokens.lift(index + offset)

  /** Advances one token and returns it. */
  private def advance(): Token =
    val tok = current
    if !isAtEnd then index += 1
    tok


  /** Returns true when parser reached EOF token. */
  private def isAtEnd: Boolean = current.kind == TokenKind.EOF

  /** Removes surrounding quotes from string tokens. */
  private def unquote(text: String): String =
    if text.length >= 2 && text.head == '"' && text.last == '"' then text.substring(1, text.length - 1) else text

  /** Merges source ranges from two tokens. */
  private def merge(a: Token, b: Token): SourceRange = a.source.merge(b.source)

  /** Merges source ranges from token and expression. */
  private def merge(a: Token, b: SourceRange): SourceRange = a.source.merge(b)

  /** Merges two source ranges into one covering both. */
  private def merge(a: SourceRange, b: SourceRange): SourceRange = a.merge(b)

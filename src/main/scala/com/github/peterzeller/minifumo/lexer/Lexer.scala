package com.github.peterzeller.minifumo.lexer

import com.github.peterzeller.minifumo.ast.{SourcePos, SourceRange}
import com.github.peterzeller.minifumo.parser.SyntaxError

import scala.collection.mutable

object Lexer:
  /** Lexes source input and applies layout processing in a second pass. */
  def tokens(input: String): Iterator[Either[SyntaxError, Token]] =
    applyLayout(lexRaw(input)).iterator

  /** Produces raw tokens including NL and SPACETAB markers used by layout. */
  private def lexRaw(input: String): Vector[Either[SyntaxError, Token]] =
    val out = mutable.ArrayBuffer.empty[Either[SyntaxError, Token]]
    var i = 0
    var line = 1
    var col = 1
    var atLineStart = true

    def pos = SourcePos(line, col)
    def peek(offset: Int = 0): Char = if i + offset < input.length then input.charAt(i + offset) else 0.toChar
    def emit(kind: TokenKind, text: String, start: SourcePos, end: SourcePos): Unit = out += Right(Token(kind, text, SourceRange(start, end)))

    def advance(): Char =
      val c = input.charAt(i)
      i += 1
      if c == '\n' then
        line += 1
        col = 1
        atLineStart = true
      else col += 1
      c

    def advanceUnit(): Unit = advance(): Unit

    while i < input.length do
      val ch = peek()
      if ch == ' ' then
        val start = pos
        var count = 0
        while peek() == ' ' do
          advance(); count += 1
        if atLineStart && count >= 2 then emit(TokenKind.SPACETAB, " " * count, start, SourcePos(line, col - 1))
      else if ch == '\t' then
        val start = pos
        advance()
        out += Left(SyntaxError(start, "Tabs are not supported for indentation."))
        emit(TokenKind.SPACETAB, "\t", start, SourcePos(line, col - 1))
      else if ch == '\r' then
        advanceUnit()
      else if ch == '\n' then
        val start = pos
        advance()
        emit(TokenKind.NL, "\\n", start, start)
      else if ch == '/' && peek(1) == '/' then
        while i < input.length && peek() != '\n' do advanceUnit()
      else if ch == '/' && peek(1) == '*' then
        val start = pos
        advance(); advance()
        var closed = false
        while i < input.length && !closed do
          if peek() == '*' && peek(1) == '/' then
            advance(); advance(); closed = true
          else advanceUnit()
        if !closed then out += Left(SyntaxError(start, "Unterminated block comment."))
      else if ch == '"' then
        val start = pos
        val b = new StringBuilder
        advance()
        var closed = false
        while i < input.length && !closed do
          val c = advance()
          if c == '"' then closed = true
          else if c == '\\' && i < input.length then
            b += c
            b += advance()
          else b += c
        if closed then emit(TokenKind.STRING, s"\"${b.result()}\"", start, SourcePos(line, col - 1))
        else out += Left(SyntaxError(start, "Unterminated string literal."))
        atLineStart = false
      else if ch.isDigit then
        val start = pos
        val b = new StringBuilder
        while peek().isDigit do b += advance()
        emit(TokenKind.INT, b.result(), start, SourcePos(line, col - 1))
        atLineStart = false
      else if ch.isLetter || ch == '_' then
        val start = pos
        val b = new StringBuilder
        while peek().isLetterOrDigit || peek() == '_' do b += advance()
        val text = b.result()
        emit(keyword(text), text, start, SourcePos(line, col - 1))
        atLineStart = false
      else
        val start = pos
        val tok =
          if s"$ch${peek(1)}${peek(2)}${peek(3)}" == "<==>" then Some((TokenKind.IFF, 4))
          else if s"$ch${peek(1)}${peek(2)}" == "==>" then Some((TokenKind.IMPLIES, 3))
          else if s"$ch${peek(1)}" == "=>" then Some((TokenKind.FAT_ARROW, 2))
          else if s"$ch${peek(1)}" == "->" then Some((TokenKind.ARROW, 2))
          else if s"$ch${peek(1)}" == "<=" then Some((TokenKind.LE, 2))
          else if s"$ch${peek(1)}" == ">=" then Some((TokenKind.GE, 2))
          else if s"$ch${peek(1)}" == "==" then Some((TokenKind.EQEQ, 2))
          else if s"$ch${peek(1)}" == "!=" then Some((TokenKind.NOTEQ, 2))
          else if s"$ch${peek(1)}" == "::" then Some((TokenKind.COLONCOLON, 2))
          else ch match
            case '(' => Some((TokenKind.PAREN_LEFT, 1))
            case ')' => Some((TokenKind.PAREN_RIGHT, 1))
            case '[' => Some((TokenKind.BRACKET_LEFT, 1))
            case ']' => Some((TokenKind.BRACKET_RIGHT, 1))
            case '{' => Some((TokenKind.BRACE_LEFT, 1))
            case '}' => Some((TokenKind.BRACE_RIGHT, 1))
            case '.' => Some((TokenKind.DOT, 1))
            case ',' => Some((TokenKind.COMMA, 1))
            case '+' => Some((TokenKind.PLUS, 1))
            case '*' => Some((TokenKind.MULT, 1))
            case '-' => Some((TokenKind.MINUS, 1))
            case '/' => Some((TokenKind.DIV, 1))
            case '%' => Some((TokenKind.MOD, 1))
            case ':' => Some((TokenKind.COLON, 1))
            case '=' => Some((TokenKind.EQ, 1))
            case '|' => Some((TokenKind.BAR, 1))
            case '<' => Some((TokenKind.LT, 1))
            case '>' => Some((TokenKind.GT, 1))
            case _ => None
        tok match
          case Some((kind, size)) =>
            val b = new StringBuilder
            (0 until size).foreach(_ => b += advance())
            emit(kind, b.result(), start, SourcePos(line, col - 1))
            atLineStart = false
          case None =>
            advance()
            out += Left(SyntaxError(start, s"Unexpected character '$ch'."))

    out += Right(Token.eof(SourcePos(line, col)))
    out.toVector

  /** Converts NL/SPACETAB stream into NL/BEGIN/END according to indentation rules. */
  private def applyLayout(tokens: Vector[Either[SyntaxError, Token]]): Vector[Either[SyntaxError, Token]] =
    enum State:
      case INIT, NEWLINES, BEGIN_LINE

    val result = mutable.ArrayBuffer.empty[Either[SyntaxError, Token]]
    val outputQueue = mutable.Queue.empty[Either[SyntaxError, Token]]
    val indentStack = mutable.Stack(0)
    var state = State.INIT
    var firstNewline: Option[Token] = None
    var numberOfSpaces = 0
    var lastCharWasWrap = false

    def isWrap(kind: TokenKind): Boolean = Set(TokenKind.COMMA, TokenKind.PLUS, TokenKind.MULT, TokenKind.MINUS, TokenKind.DIV,
      TokenKind.MOD, TokenKind.AND, TokenKind.OR, TokenKind.COLON, TokenKind.COLONCOLON, TokenKind.EQ, TokenKind.EQEQ,
      TokenKind.NOTEQ, TokenKind.BAR, TokenKind.IMPLIES, TokenKind.IFF).contains(kind)

    def isWrapEndLine(kind: TokenKind): Boolean = Set(TokenKind.PAREN_LEFT, TokenKind.BRACKET_LEFT, TokenKind.BRACE_LEFT).contains(kind) || isWrap(kind)
    def isWrapBeginLine(kind: TokenKind): Boolean = Set(TokenKind.PAREN_RIGHT, TokenKind.BRACKET_RIGHT, TokenKind.BRACE_RIGHT, TokenKind.NOT, TokenKind.DOT).contains(kind) || isWrap(kind)

    def handleIndent(n: Int, token: Token): Unit =
      if n > indentStack.top then
        indentStack.push(n)
        outputQueue.enqueue(Right(Token(TokenKind.BEGIN, "$begin", token.source)))
      else
        while n < indentStack.top do
          indentStack.pop()
          outputQueue.enqueue(Right(Token(TokenKind.END, "$end", SourceRange(token.source.start, token.source.start))))
        if n != indentStack.top then outputQueue.enqueue(Left(SyntaxError(token.source.start, s"Invalid indentation level. Expected ${indentStack.top}, got $n.")))

    def flushQueue(): Unit =
      while outputQueue.nonEmpty do result += outputQueue.dequeue()

    tokens.foreach {
      case Left(err) => result += Left(err)
      case Right(token) =>
        if token.kind == TokenKind.EOF then
          handleIndent(0, token)
          outputQueue.enqueue(Right(Token(TokenKind.NL, "$NL", token.source)))
          outputQueue.enqueue(Right(token))
          flushQueue()
        else
          state match
            case State.INIT =>
              if token.kind == TokenKind.NL then
                if !lastCharWasWrap then
                  firstNewline = Some(token)
                  state = State.NEWLINES
              else if token.kind != TokenKind.SPACETAB then
                lastCharWasWrap = isWrapEndLine(token.kind)
                result += Right(token)
            case State.NEWLINES =>
              if isWrapBeginLine(token.kind) then
                lastCharWasWrap = isWrap(token.kind)
                state = State.INIT
                result += Right(token)
              else if token.kind == TokenKind.NL then ()
              else if token.kind == TokenKind.SPACETAB then
                numberOfSpaces = token.text.length
                state = State.BEGIN_LINE
              else
                handleIndent(0, token)
                result += Right(firstNewline.get)
                flushQueue()
                result += Right(token)
                state = State.INIT
            case State.BEGIN_LINE =>
              if token.kind == TokenKind.SPACETAB then numberOfSpaces += token.text.length
              else if token.kind == TokenKind.NL then state = State.NEWLINES
              else if isWrapBeginLine(token.kind) then
                lastCharWasWrap = isWrap(token.kind)
                state = State.INIT
                result += Right(token)
              else if lastCharWasWrap && numberOfSpaces > indentStack.top then
                state = State.INIT
                result += Right(token)
              else
                handleIndent(numberOfSpaces, token)
                result += Right(firstNewline.get)
                flushQueue()
                result += Right(token)
                state = State.INIT
    }
    result.toVector

  /** Maps identifiers to keyword token kinds. */
  private def keyword(text: String): TokenKind =
    text match
      case "import" => TokenKind.IMPORT
      case "from" => TokenKind.FROM
      case "in" => TokenKind.IN
      case "export" => TokenKind.EXPORT
      case "data" => TokenKind.DATA
      case "fun" => TokenKind.FUN
      case "match" => TokenKind.MATCH
      case "case" => TokenKind.CASE
      case "if" => TokenKind.IF
      case "then" => TokenKind.THEN
      case "else" => TokenKind.ELSE
      case "let" => TokenKind.LET
      case "forall" => TokenKind.FORALL
      case "exists" => TokenKind.EXISTS
      case "and" => TokenKind.AND
      case "or" => TokenKind.OR
      case "not" => TokenKind.NOT
      case "div" => TokenKind.DIV
      case "mod" => TokenKind.MOD
      case "true" | "false" => TokenKind.BOOL
      case _ => TokenKind.ID

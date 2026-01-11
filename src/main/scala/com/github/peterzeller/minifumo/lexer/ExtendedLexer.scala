package com.github.peterzeller.minifumo.lexer

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.misc.Pair
import java.util

import com.github.peterzeller.minifumo.antlr.MinifumoLexer
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala
import com.github.peterzeller.minifumo.lexer.ExtendedLexer.ExtendedMinifumoLexer.State

object ExtendedLexer:

  object WLogger:
    def info(str: String): Unit =
      println(str)

    def trace(str: String): Unit =
      println(str)

  object ExtendedMinifumoLexer:
    enum State:
      case INIT, NEWLINES, BEGIN_LINE

  class ExtendedMinifumoLexer(val input: CharStream) extends TokenSource:
    final private val orig: MinifumoLexer = new MinifumoLexer(input)
    private val sourcePair: Pair[TokenSource, CharStream] = new Pair[TokenSource, CharStream](orig, input)
    private val indentationLevels: mutable.Stack[Integer] = new mutable.Stack[Integer]
    indentationLevels.push(0)
    private val nextTokens: util.Queue[Token] = new util.LinkedList[Token]
    private var state: State = ExtendedMinifumoLexer.State.INIT

    private var spacesPerIndent: Int = -(1)
    private var eof: Option[Token] = None
    private var firstNewline: Option[Token] = None
    private var numberOfTabs: Int = 0
    //    private val lineOffsets: LineOffsets = new LineOffsets
    private val debug: Boolean = false
    private var lastCharWasWrap: Boolean = false
    private var lastToken: Option[Token] = None
    // which character is used for indentation
    // counts the number of open parentheses
    private var parenthesesLevel: Int = 0

    override def getCharPositionInLine: Int =
      orig.getCharPositionInLine

    override def getInputStream: CharStream =
      orig.getInputStream

    override def getLine: Int =
      orig.getLine

    override def getSourceName: String =
      orig.getSourceName

    override def getTokenFactory: TokenFactory[?] =
      orig.getTokenFactory

    override def nextToken: Token =
      val t: Token = nextTokenIntern
      lastToken = Some(t)
      if debug then
        WLogger.trace(
          "     new token  = " + MinifumoParser.VOCABULARY.getSymbolicName(t.getType) + " '" + t.getText + "'"
        )
      t

    def addErrorListener(errorListener: ANTLRErrorListener): Unit =
      orig.addErrorListener(errorListener)

    private def nextTokenIntern: Token =
      if !nextTokens.isEmpty then
        return nextTokens.poll
      val l_eof: Option[Token] = eof
      for eof <- l_eof do
        return makeToken(Token.EOF, "$EOF", eof.getStartIndex, eof.getStopIndex)

      def continue(): Token =
        val token1: Token = orig.nextToken
        if debug then
          WLogger.info(
            s"$state orig token = " + MinifumoParser.VOCABULARY.getSymbolicName(token1.getType) + " '" + token1.getText + "'"
          )
        if token1 == null then
          return null
        val token: Token = token1
        if token.getType == MinifumoParser.NL then
          var line: Int = 0
          for i <- 0 until token.getText.length do
            val c: Char = token.getText.charAt(i)
            if c == '\n' then
              line += 1
        else
          if token.getType == MinifumoParser.PAREN_LEFT then
            parenthesesLevel += 1
          else if token.getType == MinifumoParser.PAREN_RIGHT then
            parenthesesLevel -= 1
          else if token.getType == Token.EOF then
            handleIndent(0, token, token.getStartIndex, token.getStopIndex, Some(token))
            eof = Some(token)
            // if inside Minifumo, add a closing newline
            nextTokens.add(makeToken(MinifumoParser.NL, "$NL", token.getStartIndex, token.getStopIndex))
            // add a single newline
            return makeToken(MinifumoParser.NL, "$NL", token.getStartIndex, token.getStopIndex)
        state match
          case State.INIT =>
            if token.getType == MinifumoParser.NL then
              if lastCharWasWrap then
                return continue()
              else
                firstNewline = Some(token)
                state(ExtendedMinifumoLexer.State.NEWLINES)
                return continue()
            else if isTab(token) then
              return continue()
            lastCharWasWrap = isWrapCharEndLine(token.getType)
            return token
          case State.NEWLINES =>
            if isWrapCharBeginLine(token.getType) then
              // ignore all the newlines when a wrap char comes after newlines
              lastCharWasWrap = isWrapChar(token.getType)
              state(ExtendedMinifumoLexer.State.INIT)
              return token
            else if token.getType == MinifumoParser.NL then
              return continue()
            else if isTab(token) then
              state(ExtendedMinifumoLexer.State.BEGIN_LINE)
              numberOfTabs = tabWidth(token)
              return continue()
            else
              handleIndent(0, token, token.getStartIndex, token.getStopIndex, firstNewline)
              nextTokens.add(token)
              state(ExtendedMinifumoLexer.State.INIT)
              return firstNewline.get
          case State.BEGIN_LINE =>
            if isTab(token) then
              numberOfTabs += tabWidth(token)
            else
              if token.getType == MinifumoParser.NL then
                state(ExtendedMinifumoLexer.State.NEWLINES)
              else if isWrapCharBeginLine(token.getType) then
                lastCharWasWrap = isWrapChar(token.getType)
                state(ExtendedMinifumoLexer.State.INIT)
                return token
              else if lastCharWasWrap && numberOfTabs > indentationLevels.top then
                state(ExtendedMinifumoLexer.State.INIT)
                return token
              else
                handleIndent(numberOfTabs, token, token.getStartIndex, token.getStopIndex, firstNewline)
                state(ExtendedMinifumoLexer.State.INIT)
                nextTokens.add(token)
                return firstNewline.get
        continue()

      continue()

    def tabWidth(token: Token): Int =
      val len: Int = 1 + token.getStopIndex - token.getStartIndex
      token.getType match
        case MinifumoParser.SPACETAB =>
          len
        case _ =>
          throw new IllegalArgumentException

    private def isTab(token: Token): Boolean =
      token.getType == MinifumoParser.SPACETAB

    private def state(s: ExtendedMinifumoLexer.State): Unit =
      if debug then
        WLogger.info("state " + state + " -> " + s)
      state = s

    private def handleIndent(n: Int, token: Token, start: Int, stop: Int, endBlockToken: Option[Token]): Unit =
      if debug then
        WLogger.info("handleIndent " + n + "\t " + indentationLevels)
      if n > indentationLevels.top then
        if spacesPerIndent < 0 then
          spacesPerIndent = n
        indentationLevels.push(n)
        nextTokens.add(makeToken(MinifumoParser.BEGIN, "$begin", start, stop))
      else
        while n < indentationLevels.top do
          indentationLevels.pop
          nextTokens.add(
            makeToken(
              MinifumoParser.END,
              "$end",
              endBlockToken.map(_.getStartIndex).getOrElse(0),
              endBlockToken.map(_.getStartIndex).getOrElse(0)
            )
          )
        val expectedIndentation: Integer = indentationLevels.top
        if n != expectedIndentation then
          val msg: String =
            "Invalid indentation level. Current indentation is " + expectedIndentation + ", but this is indented by " + n + "."
          for el <- orig.getErrorListeners.asScala do
            val line = token.getLine
            el.syntaxError(orig, "", line, token.getCharPositionInLine, msg, null)

    private def isWrapChar(tokenKind: Int): Boolean =
      tokenKind match
        case MinifumoParser.COMMA
            | MinifumoParser.PLUS
            | MinifumoParser.MULT
            | MinifumoParser.MINUS
            | MinifumoParser.DIV
            | MinifumoParser.MOD
            | MinifumoParser.AND
            | MinifumoParser.OR
            | MinifumoParser.COLON
            | MinifumoParser.COLONCOLON
            | MinifumoParser.EQ
            | MinifumoParser.EQEQ
            | MinifumoParser.NOTEQ
            | MinifumoParser.BAR
            | MinifumoParser.IMPLIES
            | MinifumoParser.IFF => true
        case _ => false

    private def isWrapCharEndLine(tokenKind: Int): Boolean =
      tokenKind match
        case MinifumoParser.PAREN_LEFT | MinifumoParser.BRACKET_LEFT | MinifumoParser.BRACE_LEFT =>
          true
        case _ =>
          isWrapChar(tokenKind)

    private def isWrapCharBeginLine(`type`: Int): Boolean =
      `type` match
        case MinifumoParser.PAREN_RIGHT
            | MinifumoParser.BRACKET_RIGHT
            | MinifumoParser.BRACE_RIGHT
            | MinifumoParser.NOT
            | MinifumoParser.DOT =>
          true
        case _ =>
          isWrapChar(`type`)

    private def makeToken(`type`: Int, text: String, start: Int, stop: Int): Token =
      val source: Pair[TokenSource, CharStream] = sourcePair
      val channel: Int = 0
      val t: CommonToken = new CommonToken(source, `type`, channel, start, stop)
      t

    override def setTokenFactory(factory: TokenFactory[?]): Unit =
      orig.setTokenFactory(factory)

    def setErrorListener(listener: ANTLRErrorListener): Unit =
      orig.removeErrorListeners()
      orig.addErrorListener(listener)

package com.github.peterzeller.minifumo.parser
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.antlr.MinifumoParser.ProgramContext
import com.github.peterzeller.minifumo.lexer.ExtendedLexer.ExtendedMinifumoLexer
import org.antlr.v4.runtime.{ANTLRErrorListener, CharStream, Parser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import java.util as ju
import com.github.peterzeller.minifumo.ast.{SourcePos, SourceRange}
import com.github.peterzeller.minifumo.common.MinifumoError

import java.nio.file.Path

case class SyntaxError(pos: SourcePos, message: String) extends MinifumoError:
  override def source: SourceRange = SourceRange(pos, pos)

def parseInput(input: String): (ProgramContext, List[SyntaxError]) =
  val charStream = org.antlr.v4.runtime.CharStreams.fromString(input)
  parseCharStream(charStream)
  
def parseFile(input: Path): (ProgramContext, List[SyntaxError]) =
  val charStream = org.antlr.v4.runtime.CharStreams.fromPath(input)
  parseCharStream(charStream)
  
def parseCharStream(charStream: CharStream): (ProgramContext, List[SyntaxError]) =  
  val lexer = new ExtendedMinifumoLexer(charStream)
  val errors = new ErrorCollector
  lexer.setErrorListener(errors)
  val tokenStream = new org.antlr.v4.runtime.CommonTokenStream(lexer)
  val parser = new MinifumoParser(tokenStream)
  parser.removeErrorListeners()
  parser.addErrorListener(errors)
  val tree = parser.program()
  val syntaxErrors = errors.syntaxErrors
  (tree, syntaxErrors)

class ErrorCollector extends ANTLRErrorListener:
  private val errors: scala.collection.mutable.ListBuffer[SyntaxError] = scala.collection.mutable.ListBuffer()
  def syntaxErrors: List[SyntaxError] = errors.toList
  override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: ju.BitSet, configs: ATNConfigSet): Unit = ()

  override def reportAmbiguity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: ju.BitSet, configs: ATNConfigSet): Unit = ()



  override def syntaxError(
      recognizer: org.antlr.v4.runtime.Recognizer[?, ?],
      offendingSymbol: Any,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: org.antlr.v4.runtime.RecognitionException
  ): Unit =
    errors += SyntaxError(SourcePos(line, charPositionInLine), msg)


  override def reportContextSensitivity(
      recognizer: org.antlr.v4.runtime.Parser,
      dfa: org.antlr.v4.runtime.dfa.DFA,
      startIndex: Int,
      stopIndex: Int,
      prediction: Int,
      configs: org.antlr.v4.runtime.atn.ATNConfigSet
  ): Unit = ()

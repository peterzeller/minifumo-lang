package com.github.peterzeller.minifumo.parser
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.antlr.MinifumoParser.ProgramContext
import com.github.peterzeller.minifumo.lexer.ExtendedLexer.ExtendedMinifumoLexer

def parseInput(input: String): ProgramContext =

  val charStream = org.antlr.v4.runtime.CharStreams.fromString(input)
  val lexer = new ExtendedMinifumoLexer(charStream)
  val tokenStream = new org.antlr.v4.runtime.CommonTokenStream(lexer)
  val parser = new MinifumoParser(tokenStream)
  val tree = parser.program()
  tree
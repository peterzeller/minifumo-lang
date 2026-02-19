package com.github.peterzeller.minifumo.builtins

import com.github.peterzeller.minifumo.ast.ProgramFile
import com.github.peterzeller.minifumo.parser.parseInput

object Standard:
  // Loads the embedded standard library source for browser builds.
  def loadStandardSource(): String =
    StandardSource.text

  // Parses the embedded standard source into an AST program.
  private def parseStandardProgram(): ProgramFile =
    val (ast, errors) = parseInput(loadStandardSource())
    if errors.nonEmpty then
      val message = errors.map(err => s"${err.pos.line}:${err.pos.column}: ${err.message}").mkString("\n")
      throw new IllegalStateException(s"Failed to parse standard library:\n$message")
    ast

  // Provides the parsed standard library program.
  lazy val standardProgram: ProgramFile =
    parseStandardProgram()

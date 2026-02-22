package com.github.peterzeller.minifumo.builtins

import com.github.peterzeller.minifumo.ast.ProgramFile
import com.github.peterzeller.minifumo.parser.parseInput

import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

object Standard:
  private val standardResourcePath = "/com/github/peterzeller/minifumo/builtins/standard.minifumo"
  private val standardLibraryPath =
    Paths.get("src/main/resources/com/github/peterzeller/minifumo/builtins/standard.minifumo")

  // Loads the standard library source from classpath resources or disk fallback.
  def loadStandardSource(): String =
    loadStandardSourceFromResources().getOrElse(loadStandardSourceFromDisk())

  // Loads the standard library source from bundled classpath resources.
  private def loadStandardSourceFromResources(): Option[String] =
    Option(getClass.getResourceAsStream(standardResourcePath)).map { stream =>
      Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)
    }

  // Loads the standard library source directly from the repository checkout.
  private def loadStandardSourceFromDisk(): String =
    if !Files.exists(standardLibraryPath) then
      throw new IllegalStateException(s"Standard library not found: ${standardLibraryPath.toString}")
    new String(Files.readAllBytes(standardLibraryPath))

  // Parses the standard library source into an AST program.
  private def parseStandardProgram(): ProgramFile =
    val (ast, errors) = parseInput(loadStandardSource())
    if errors.nonEmpty then
      val message = errors.map(err => s"${err.pos.line}:${err.pos.column}: ${err.message}").mkString("\n")
      throw new IllegalStateException(s"Failed to parse standard library:\n$message")
    ast

  // Provides the parsed standard library program.
  lazy val standardProgram: ProgramFile =
    parseStandardProgram()

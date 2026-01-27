package com.github.peterzeller.minifumo.builtins

import com.github.peterzeller.minifumo.ast.{AstTransform, ProgramFile}
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{TypeChecker, TypedAst}

import java.nio.file.{Files, Paths}

object Standard:
  private val standardLibraryPath =
    Paths.get("src/main/scala/com/github/peterzeller/minifumo/builtins/standard.minifumo")

  // Loads the standard library source file from disk.
  private def loadStandardSource(): String =
    if !Files.exists(standardLibraryPath) then
      throw new IllegalStateException(s"Standard library not found: ${standardLibraryPath.toString}")
    new String(Files.readAllBytes(standardLibraryPath))

  // Parses the standard library source into an AST program.
  private def parseStandardProgram(): ProgramFile =
    val (cst, errors) = parseInput(loadStandardSource())
    if errors.nonEmpty then
      val message = errors.map(err => s"${err.pos.line}:${err.pos.column}: ${err.message}").mkString("\n")
      throw new IllegalStateException(s"Failed to parse standard library:\n$message")
    AstTransform.program(cst)

  // Provides the parsed standard library program.
  lazy val standardProgram: ProgramFile =
    parseStandardProgram()

  // Provides the export environment for the standard library.
  lazy val standardExports: TypeChecker.ExportEnv =
    val (exports, errors) =
      TypeChecker.extractExports(standardProgram, TypeChecker.emptyExportEnv, includeNonExported = true)
    if errors.nonEmpty then
      val message = errors.map(_.message).mkString("\n")
      throw new IllegalStateException(s"Failed to extract standard library exports:\n$message")
    exports

  // Provides the typed standard library program for runtime evaluation.
  lazy val typedProgram: TypedAst.Program =
    val (typed, errors) = TypeChecker.checkProgramWithoutStandard(standardProgram, TypeChecker.emptyExportEnv)
    if errors.nonEmpty then
      val message = errors.map(_.message).mkString("\n")
      throw new IllegalStateException(s"Failed to type-check standard library:\n$message")
    typed

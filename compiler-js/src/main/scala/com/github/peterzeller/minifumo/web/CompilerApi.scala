package com.github.peterzeller.minifumo.web

import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, ProjectSymbolCache, TypeChecker}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.control.NonFatal

/** Exposes compiler and interpreter helpers for the browser frontend. */
@JSExportTopLevel("MinifumoCompiler")
object CompilerApi:

  private val inMemoryFile: String = "/playground/input.minifumo"

  /** Compiles source code and optionally runs a named function from the resulting program. */
  @JSExport
  def compileAndRun(source: String, functionName: String = "main", runFunction: Boolean = true): js.Object =
    val ids = TypeChecker.IdSupply()
    val symbolCache = ProjectSymbolCache(new GlobalSymbolsIo("."), ids)
    symbolCache.addInput(inMemoryFile, source)
    try
      val (program, syntaxErrors) = parseInput(source)
      if syntaxErrors.nonEmpty then
        compileResult(
          success = false,
          output = "",
          errors = syntaxErrors.map(errorFromSyntax).toJSArray,
          typed = false,
          executed = false
        )
      else
        val (typedProgram, typeErrors) = TypeChecker.checkProgram(inMemoryFile, program, symbolCache, importStandard = true, ids)
        if typeErrors.nonEmpty then
          compileResult(
            success = false,
            output = "",
            errors = typeErrors.map(errorFromType).toJSArray,
            typed = false,
            executed = false
          )
        else if runFunction then
          val value = Interpreter.evalProg(typedProgram, List(typedStandardProgram), functionName)
          compileResult(
            success = true,
            output = value.toString,
            errors = js.Array(),
            typed = true,
            executed = true
          )
        else
          compileResult(
            success = true,
            output = s"Compilation successful. Function '${functionName}' is ready.",
            errors = js.Array(),
            typed = true,
            executed = false
          )
    catch
      // Converts unexpected runtime/compiler exceptions into frontend-visible diagnostics.
      case NonFatal(error) =>
        compileResult(
          success = false,
          output = "",
          errors = js.Array(genericError(error)),
          typed = false,
          executed = false
        )

  // Type-checks the bundled standard library once for runtime evaluation support.
  private lazy val typedStandardProgram =
    val ids = TypeChecker.IdSupply()
    val symbolCache = ProjectSymbolCache(new GlobalSymbolsIo("."), ids)
    val (typedStandard, errors) = TypeChecker.checkProgram("standard.minifumo", Standard.standardProgram, symbolCache, importStandard = false, ids)
    if errors.nonEmpty then
      val errorMessage = errors.map(_.message).mkString("\n")
      throw new IllegalStateException(s"Failed to type-check standard library:\n$errorMessage")
    typedStandard

  // Converts syntax errors into structured data for frontend rendering.
  private def errorFromSyntax(error: com.github.peterzeller.minifumo.parser.SyntaxError): js.Object =
    js.Dynamic.literal(
      message = error.message,
      line = error.pos.line,
      column = error.pos.column,
      endColumn = error.pos.column + 1
    )

  // Converts type errors into structured data for frontend rendering.
  private def errorFromType(error: TypeChecker.TypeError): js.Object =
    js.Dynamic.literal(
      message = error.message,
      line = error.source.start.line,
      column = error.source.start.column,
      endColumn = error.source.end.column
    )

  // Converts generic thrown exceptions into a consistent frontend error object.
  private def genericError(error: Throwable): js.Object =
    js.Dynamic.literal(
      message = Option(error.getMessage).getOrElse(error.toString),
      line = 1,
      column = 1,
      endColumn = 1
    )

  // Builds a frontend-friendly compile response payload.
  private def compileResult(success: Boolean, output: String, errors: js.Array[js.Object], typed: Boolean, executed: Boolean): js.Object =
    js.Dynamic.literal(
      success = success,
      output = output,
      errors = errors,
      typed = typed,
      executed = executed
    )


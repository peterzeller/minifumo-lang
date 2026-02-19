package com.github.peterzeller.minifumo.web

import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{GlobalName, GlobalSymbol, NameCache, SymbolCache, TypeChecker}

import java.nio.file.{Path, Paths}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Exposes compiler and interpreter helpers for the browser frontend. */
@JSExportTopLevel("MinifumoCompiler")
object CompilerApi:

  private val inMemoryFile = Paths.get("web-input.minifumo")

  /** Compiles source code and optionally runs a named function from the resulting program. */
  @JSExport
  def compileAndRun(source: String, functionName: String = "main", runFunction: Boolean = true): js.Object =
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
      val ids = TypeChecker.IdSupply()
      val (typedProgram, typeErrors) = TypeChecker.checkProgram(inMemoryFile, program, EmptyCache, importStandard = true, ids)
      if typeErrors.nonEmpty then
        compileResult(
          success = false,
          output = "",
          errors = typeErrors.map(errorFromType).toJSArray,
          typed = false,
          executed = false
        )
      else if runFunction then
        val value = Interpreter.evalProg(typedProgram, functionName)
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

  // Converts syntax errors into structured data for frontend rendering.
  private def errorFromSyntax(error: com.github.peterzeller.minifumo.parser.SyntaxError): js.Object =
    js.Dynamic.literal(
      message = error.message,
      line = error.pos.line,
      column = error.pos.column
    )

  // Converts type errors into structured data for frontend rendering.
  private def errorFromType(error: TypeChecker.TypeError): js.Object =
    js.Dynamic.literal(
      message = error.message,
      line = error.source.start.line,
      column = error.source.start.column
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

  // Minimal cache implementation for browser mode where imports are disabled.
  private object EmptyCache extends NameCache with SymbolCache:
    override def globalNames(path: String): Map[String, GlobalName] =
      Map.empty

    override def globalSymbols(path: String): Map[String, GlobalSymbol] =
      Map.empty

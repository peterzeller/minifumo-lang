package com.github.peterzeller.minifumo.web

import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.ast.SourcePos
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{DefinitionLookup, GlobalSymbolsIo, HoverLookup, ProjectSymbolCache, TypeChecker}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.control.NonFatal

/** Exposes compiler and interpreter helpers for the browser frontend. */
@JSExportTopLevel("MinifumoCompiler")
object CompilerApi:

  private val inMemoryFile: String = "/playground/input.minifumo"

  /** Represents one structured compiler or runtime error sent to JavaScript callers. */
  class CompileError(val message: String, val line: Int, val column: Int, val endColumn: Int) extends js.Object

  /** Represents the compile/eval result payload sent to JavaScript callers. */
  class CompileResult(val success: Boolean, val output: String, val errors: js.Array[CompileError], val typed: Boolean, val executed: Boolean)
      extends js.Object

  /** Represents one go-to-definition target location sent to JavaScript callers. */
  class DefinitionLocation(val file: String, val line: Int, val column: Int, val endLine: Int, val endColumn: Int) extends js.Object

  /** Represents hover information at one source position. */
  class HoverInfo(val typeText: String, val comment: js.UndefOr[String]) extends js.Object

  /** Compiles source code and optionally runs a named function from the resulting program. */
  @JSExport
  def compileAndRun(source: String, functionName: String = "main", runFunction: Boolean = true): CompileResult =
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

  /** Resolves the definition location at the given 1-based source position. */
  @JSExport
  def definitionAt(source: String, line: Int, column: Int, currentFile: String = inMemoryFile): js.UndefOr[DefinitionLocation] =
    val ids = TypeChecker.IdSupply()
    val symbolCache = ProjectSymbolCache(new GlobalSymbolsIo("."), ids)
    symbolCache.addInput(currentFile, source)
    try
      val (program, syntaxErrors) = parseInput(source)
      if syntaxErrors.nonEmpty then
        js.undefined
      else
        val (typedProgram, _) = TypeChecker.checkProgram(currentFile, program, symbolCache, importStandard = true, ids)
        DefinitionLookup
          .definitionAt(typedProgram, SourcePos(line, column), currentFile)
          .map(location =>
            new DefinitionLocation(
              location.file,
              location.range.start.line,
              location.range.start.column,
              location.range.end.line,
              location.range.end.column
            )
          )
          .orUndefined
    catch
      // Keeps go-to-definition resilient by hiding internal compiler failures from callers.
      case NonFatal(_) =>
        js.undefined


  /** Resolves hover information at the given 1-based source position. */
  @JSExport
  def hoverAt(source: String, line: Int, column: Int, currentFile: String = inMemoryFile): js.UndefOr[HoverInfo] =
    val ids = TypeChecker.IdSupply()
    val symbolCache = ProjectSymbolCache(new GlobalSymbolsIo("."), ids)
    symbolCache.addInput(currentFile, source)
    try
      val (program, syntaxErrors) = parseInput(source)
      if syntaxErrors.nonEmpty then
        js.undefined
      else
        val (typedProgram, _) = TypeChecker.checkProgram(currentFile, program, symbolCache, importStandard = true, ids)
        HoverLookup.hoverAt(typedProgram, SourcePos(line, column), currentFile)
          .map(info => new HoverInfo(info.typeText, info.comment.orUndefined))
          .orUndefined
    catch
      case NonFatal(_) =>
        js.undefined

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
  private def errorFromSyntax(error: com.github.peterzeller.minifumo.parser.SyntaxError): CompileError =
    new CompileError(error.message, error.pos.line, error.pos.column, error.pos.column + 1)

  // Converts type errors into structured data for frontend rendering.
  private def errorFromType(error: TypeChecker.TypeError): CompileError =
    new CompileError(error.message, error.source.start.line, error.source.start.column, error.source.end.column)

  // Converts generic thrown exceptions into a consistent frontend error object.
  private def genericError(error: Throwable): CompileError =
    new CompileError(Option(error.getMessage).getOrElse(error.toString), 1, 1, 1)

  // Builds a frontend-friendly compile response payload.
  private def compileResult(success: Boolean, output: String, errors: js.Array[CompileError], typed: Boolean, executed: Boolean): CompileResult =
    new CompileResult(success, output, errors, typed, executed)

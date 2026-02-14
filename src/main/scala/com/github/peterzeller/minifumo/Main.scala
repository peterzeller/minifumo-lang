package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, ProgramFile, SourcePos, SourceRange}
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.{MinifumoError, MinifumoErrorWithPath}
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseInput}
import com.github.peterzeller.minifumo.typing.{ProjectSymbolCache, TypeChecker, findProjectRoot}
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

object Main:

  // Entry point for the CLI.
  def main(args: Array[String]): Unit =
    args.toList match
      case "run" :: filename :: Nil =>
        val path = Paths.get(filename)
        if Files.isDirectory(path) then
          Console.err.println(s"minifumo run expects a file, got directory: $filename")
          System.exit(2)
        runFile(path) match
          case Right(value) => println(value)
          case Left(messages) =>
            messages.foreach(Console.err.println)
            System.exit(1)
      case "check" :: filename :: Nil =>
        val path = Paths.get(filename)
        val failures =
          if Files.isDirectory(path) then
            checkDirectory(path)
          else
            checkFile(path)
        if failures.nonEmpty then
          failures.foreach(Console.err.println)
          System.exit(1)
      case _ =>
        println(s"Unknown command ${args.mkString(" ")}")
        printUsage()
        System.exit(2)

  // Prints command-line usage information.
  private def printUsage(): Unit =
    Console.err.println(
      """Usage:
        |  minifumo run <filename>
        |  minifumo check <filename-or-directory>""".stripMargin
    )

  // Runs a program file and returns either error messages or the evaluated value.
  def runFile(path: Path): Either[List[MinifumoErrorWithPath], Interpreter.Value] =
    val globalNames = new ProjectSymbolCache(findProjectRoot(path))
    val (_, syntaxErrors) = globalNames.getAst(globalNames.fromPath(path))
    if syntaxErrors.nonEmpty then
      Left(syntaxErrors.map(MinifumoErrorWithPath(path, _)))
    else
      // val (typedProgram, typeErrors) = TypeChecker.checkProgram(path, program, globalNames, true)
      val (typedProgram, _) = globalNames.typedAst(globalNames.fromPath(path))
      val allErrors = globalNames.allErrors

      if allErrors.nonEmpty then
        throw new RuntimeException(s"errros: $allErrors")
        // Left(renderTypeErrors(path, typeErrors))
      else
        Right(Interpreter.evalProg(typedProgram, globalNames, "main"))

  // Checks a directory of examples, reusing cached parse/import info across files.
  def checkDirectory(path: Path): List[MinifumoErrorWithPath] =
    if !Files.exists(path) then
      List(MinifumoErrorWithPath(path, SyntaxError(SourcePos(0,0), "Directory not found")))
    else
      val globalNames = new ProjectSymbolCache(findProjectRoot(path))
      Try {
        Using.resource(Files.list(path)) { stream =>
          stream.iterator().asScala.toList
            .filter(Files.isRegularFile(_))
            .filter(_.toString.endsWith(".minifumo"))
            .sortBy(_.toString)
            .flatMap(checkFile(_, globalNames))
        }
      }.getOrElse(List(MinifumoErrorWithPath(path, SyntaxError(SourcePos(0,0), "Could not read path"))))

  // Checks a single file for syntax and type errors, including imports.
  def checkFile(path: Path): List[MinifumoErrorWithPath] =
    val globalNames = new ProjectSymbolCache(findProjectRoot(path))
    checkFile(path, globalNames)

  // Checks a file using shared caches to avoid reparsing across a project.
  private def checkFile(path: Path, info: ProjectSymbolCache): List[MinifumoErrorWithPath] =
    val (program, syntaxErrors) = loadProgram(path, info)
    if syntaxErrors.nonEmpty then
      syntaxErrors.map(MinifumoErrorWithPath(path, _))
    else
      val (_, errors) = TypeChecker.checkProgram(path, program, info, true)
      if errors.isEmpty then
        Nil
      else
        errors.map(MinifumoErrorWithPath(path, _))

  // Represents an empty program when parsing fails.
  private val emptyProgramFile = ProgramFile(List(), List())(SourceRange(SourcePos(0, 0), SourcePos(0, 0)))

  // Loads and caches syntax parsing results for a file path.
  private def loadProgram(path: Path, info: ProjectSymbolCache): (ProgramFile, List[SyntaxError]) =
    info.getAst(info.makeRelative(path))

  // Renders a source range for error reporting.
  private def renderSourceRange(range: SourceRange): String =
    val start = range.start
    val end = range.end
    if start == end then
      s"${start.line}:${start.column}"
    else
      s"${start.line}:${start.column}-${end.line}:${end.column}"


  // Formats a list of errors for reporting.
  def renderTypeErrors(path: Path, errors: List[MinifumoError]): List[String] =
    renderTypeErrors(errors.map(MinifumoErrorWithPath(path, _)))

  def renderTypeErrors(errors: List[MinifumoErrorWithPath]): List[String] =
    var linesCache = Map[Path, Vector[String]]()
    def getLines(path: Path): Vector[String] =
      linesCache.get(path) match
        case None =>
          val r = readLines(path)
          linesCache += path -> r
          r
        case Some(r) =>
          r



    errors.map { errorWithPath =>
      val error = errorWithPath.err
      val lineIndex = error.source.start.line - 1
      val lines = getLines(errorWithPath.p)
      if lineIndex >= 0 && lineIndex < lines.length then
        val sourceLine = lines(lineIndex)
        val startColumn = math.max(1, error.source.start.column)
        val endColumn = math.max(startColumn, error.source.end.column)
        val underlineWidth = math.max(1, endColumn - startColumn + 1)
        val underline = (" " * (startColumn - 1)) + ("^" * underlineWidth)
        s"line ${error.source.start.line}: ${error.message}\n    ${sourceLine}\n    ${underline}"
      else
        s"line ${error.source.start.line}: ${error.message}"
    }

def readLines(path: Path): Vector[String] =
  try
    if path.endsWith("standard.minifumo") then
      Standard.loadStandardSource().lines().toList.asScala.toVector
    else
      Files.readAllLines(path).asScala.toVector
  catch
    case e: Exception =>
      Vector()

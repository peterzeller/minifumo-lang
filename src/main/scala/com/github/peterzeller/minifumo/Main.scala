package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, ProgramFile, SourcePos, SourceRange}
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
  def runFile(path: Path): Either[List[String], Interpreter.Value] =
    val globalNames = new ProjectSymbolCache(findProjectRoot(path))
    val (_, syntaxErrors) = globalNames.getAst(globalNames.fromPath(path))
    if syntaxErrors.nonEmpty then
      Left(renderSyntaxErrors(path, syntaxErrors))
    else
      // val (typedProgram, typeErrors) = TypeChecker.checkProgram(path, program, globalNames, true)
      val (typedProgram, typeErrors) = globalNames.typedAst(globalNames.fromPath(path))
      if typeErrors.nonEmpty then
        Left(renderTypeErrors(path, typeErrors))
      else
        Right(Interpreter.evalProg(typedProgram, globalNames, "main"))

  // Checks a directory of examples, reusing cached parse/import info across files.
  def checkDirectory(path: Path): List[String] =
    if !Files.exists(path) then
      List(s"Directory not found: ${path.toString}")
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
      }.getOrElse(List(s"Failed reading directory: ${path.toString}"))

  // Checks a single file for syntax and type errors, including imports.
  def checkFile(path: Path): List[String] =
    val globalNames = new ProjectSymbolCache(findProjectRoot(path))
    checkFile(path, globalNames)

  // Checks a file using shared caches to avoid reparsing across a project.
  private def checkFile(path: Path, info: ProjectSymbolCache): List[String] =
    val (program, syntaxErrors) = loadProgram(path, info)
    if syntaxErrors.nonEmpty then
      renderSyntaxErrors(path, syntaxErrors)
    else
      val (_, errors) = TypeChecker.checkProgram(path, program, info, true)
      if errors.isEmpty then
        Nil
      else
        errors.map(err => s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}")

  // Represents an empty program when parsing fails.
  private val emptyProgramFile = ProgramFile(List(), List())(SourceRange(SourcePos(0, 0), SourcePos(0, 0)))

  // Parses a program file into an AST and syntax errors.
  private def parseProgram(path: Path): (ProgramFile, List[SyntaxError]) =
    if !Files.exists(path) then
      (emptyProgramFile, List(SyntaxError(SourcePos(0, 0), s"File not found: ${path.toString}")))
    else
      val content = Try {
        Using.resource(scala.io.Source.fromFile(path.toFile))(_.mkString)
      }
      content match
        case scala.util.Failure(exception) =>
          (emptyProgramFile, List(SyntaxError(SourcePos(0, 0), s"Failed reading file ${path.toString}: ${exception.getMessage}")) )
        case scala.util.Success(input) =>
          val (cst, syntaxErrors) = parseInput(input)
          val ast = AstTransform.program(cst)
          (ast, syntaxErrors)


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

  // Formats syntax errors with the given file path.
  private def renderSyntaxErrors(path: Path, errors: List[SyntaxError]): List[String] =
    errors.map { err =>
      s"${path.toString}:${err.pos.line}:${err.pos.column}: ${err.message}"
    }

  // Formats type errors with the given file path.
  private def renderTypeErrors(path: Path, errors: List[TypeError]): List[String] =
    errors.map { err =>
      s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}"
    }

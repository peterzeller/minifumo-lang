package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, ProgramFile, SourcePos, SourceRange}
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseInput}
import com.github.peterzeller.minifumo.typing.{TypeChecker, TypedAst}
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.util.{Try, Using}

object Main:
  // Stores shared caches for parsing and export resolution across multiple files.
  final case class GlobalInfo(
      parseCache: mutable.Map[Path, (ProgramFile, List[SyntaxError])],
      exportCache: mutable.Map[Path, TypeChecker.ExportEnv],
      resolvedExportCache: mutable.Map[Path, (TypeChecker.ExportEnv, List[TypeError])],
      typedProgramCache: mutable.Map[Path, (TypedAst.Program, List[TypeError])],
      resolving: mutable.Set[Path]
    )

  // Creates a new cache container for resolving imports within a project.
  private def newGlobalInfo(): GlobalInfo =
    GlobalInfo(mutable.Map.empty, mutable.Map.empty, mutable.Map.empty, mutable.Map.empty, mutable.Set.empty)

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
    val (program, syntaxErrors) = parseProgram(path)
    if syntaxErrors.nonEmpty then
      Left(renderSyntaxErrors(path, syntaxErrors))
    else
      val (typedProgram, typeErrors) = TypeChecker.checkProgram(program, TypeChecker.emptyExportEnv)
      if typeErrors.nonEmpty then
        Left(renderTypeErrors(path, typeErrors))
      else
        val combined = TypedAst.Program(Standard.typedProgram.items ++ typedProgram.items)(program.source)
        Right(Interpreter.evalProg(combined, "main"))

  // Checks a directory of examples, reusing cached parse/import info across files.
  def checkDirectory(path: Path): List[String] =
    if !Files.exists(path) then
      List(s"Directory not found: ${path.toString}")
    else
      val info = newGlobalInfo()
      Try {
        Using.resource(Files.list(path)) { stream =>
          stream.iterator().asScala.toList
            .filter(Files.isRegularFile(_))
            .filter(_.toString.endsWith(".minifumo"))
            .sortBy(_.toString)
            .flatMap(checkFile(_, info))
        }
      }.getOrElse(List(s"Failed reading directory: ${path.toString}"))

  // Checks a single file for syntax and type errors, including imports.
  def checkFile(path: Path): List[String] =
    checkFile(path, newGlobalInfo())

  // Checks a file using shared caches to avoid reparsing across a project.
  private def checkFile(path: Path, info: GlobalInfo): List[String] =
    val (program, syntaxErrors) = loadProgram(path, info)
    if syntaxErrors.nonEmpty then
      renderSyntaxErrors(path, syntaxErrors)
    else
      val (_, errors) = TypeChecker.checkProgram(program, TypeChecker.emptyExportEnv)
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
  private def loadProgram(path: Path, info: GlobalInfo): (ProgramFile, List[SyntaxError]) =
    info.parseCache.getOrElseUpdate(path, parseProgram(path))

  // Resolves an import path relative to the project root, enforcing the .minifumo extension.
  private def resolveImportPath(root: Path, pathText: String): Path =
    val rawPath = Paths.get(pathText)
    val withExtension =
      if rawPath.toString.endsWith(".minifumo") then rawPath else Paths.get(s"${rawPath.toString}.minifumo")
    root.resolve(withExtension).normalize()

  // Finds the project root by walking up to locate minifumo.yml.
  private def findProjectRoot(start: Path): Option[Path] =
    Iterator.iterate(start)(_.getParent).takeWhile(_ != null).find { candidate =>
      Files.exists(candidate.resolve("minifumo.yml"))
    }

  // Renders a source range for error reporting.
  private def renderSourceRange(range: com.github.peterzeller.minifumo.ast.SourceRange): String =
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

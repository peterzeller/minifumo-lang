package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{ProgramFile, SourcePos, SourceRange}
import com.github.peterzeller.minifumo.backends.lean.LeanBackend
import com.github.peterzeller.minifumo.backends.scala.ScalaBackend
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.{MinifumoError, MinifumoErrorWithPath}
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.SyntaxError
import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, ProjectSymbolCache, TypeChecker}

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
          case Left(errs) =>
            println(renderTypeErrors(errs).mkString("\n\n"))
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
      case "compileToLean" :: filename :: Nil =>
        val path = Paths.get(filename)
        val outputDir =
          if Files.isDirectory(path) then path.resolve(".minifumo-lean")
          else Option(path.getParent).getOrElse(Paths.get(".")).resolve(".minifumo-lean")
        compileToLean(path, outputDir)
      case "compileToLean" :: filename :: outputDir :: Nil =>
        compileToLean(Paths.get(filename), Paths.get(outputDir))
      case "compileToScala" :: filename :: Nil =>
        val path = Paths.get(filename)
        val outputDir =
          if Files.isDirectory(path) then path.resolve(".minifumo-scala")
          else Option(path.getParent).getOrElse(Paths.get(".")).resolve(".minifumo-scala")
        compileToScala(path, outputDir)
      case "compileToScala" :: filename :: outputDir :: Nil =>
        compileToScala(Paths.get(filename), Paths.get(outputDir))
      case _ =>
        println(s"Unknown command ${args.mkString(" ")}")
        printUsage()
        System.exit(2)

  // Prints command-line usage information.
  private def printUsage(): Unit =
    Console.err.println(
      """Usage:
        |  minifumo run <filename>
        |  minifumo check <filename-or-directory>
        |  minifumo compileToLean <filename-or-directory> [output-directory]
        |  minifumo compileToScala <filename-or-directory> [output-directory]""".stripMargin
    )

  // Compiles a Minifumo project to Lean and verifies generated files with Lean.
  private def compileToLean(path: Path, outputDir: Path): Unit =
    LeanBackend.compileAndCheck(path, outputDir) match
      case Left(errors) =>
        Console.err.println(renderTypeErrors(errors).mkString("\n\n"))
        System.exit(1)
      case Right(result) =>
        println(s"Generated ${result.files.length} Lean files in ${outputDir}")

  // Compiles a Minifumo project to Scala wrapper sources.
  private def compileToScala(path: Path, outputDir: Path): Unit =
    ScalaBackend.compile(path, outputDir) match
      case Left(errors) =>
        Console.err.println(renderTypeErrors(errors).mkString("\n\n"))
        System.exit(1)
      case Right(result) =>
        println(s"Generated ${result.files.length} Scala files in ${outputDir}")

  // Runs a program file and returns either error messages or the evaluated value.
  def runFile(path: Path): Either[List[MinifumoErrorWithPath], Interpreter.Value] =
    val idSupply = TypeChecker.IdSupply()
    val io = GlobalSymbolsIo(GlobalSymbolsIo.findProjectRoot(path))
    val globalNames = new ProjectSymbolCache(io, idSupply)
    val (_, syntaxErrors) = globalNames.getAst(globalNames.io.fromPath(path))
    if syntaxErrors.nonEmpty then
      Left(syntaxErrors.map(MinifumoErrorWithPath(path.toString, _)))
    else
      // val (typedProgram, typeErrors) = TypeChecker.checkProgram(path, program, globalNames, true)
      val (typedProgram, _) = globalNames.typedAst(globalNames.io.fromPath(path))
      val allErrors = globalNames.allErrors

      if allErrors.nonEmpty then
         Left(allErrors)
      else
        Right(Interpreter.evalProg(typedProgram, globalNames, "main"))

  // Checks a directory of examples, reusing cached parse/import info across files.
  def checkDirectory(path: Path): List[MinifumoErrorWithPath] =
    val idSupply = TypeChecker.IdSupply()
    if !Files.exists(path) then
      List(MinifumoErrorWithPath(path.toString, SyntaxError(SourcePos(0,0), "Directory not found")))
    else
      val io = GlobalSymbolsIo(GlobalSymbolsIo.findProjectRoot(path))
      val globalNames = new ProjectSymbolCache(io, idSupply)
      Try {
        Using.resource(Files.list(path)) { stream =>
          stream.iterator().asScala.toList
            .filter(Files.isRegularFile(_))
            .filter(_.toString.endsWith(".minifumo"))
            .sortBy(_.toString)
            .flatMap(checkFile(_, globalNames))
        }
      }.getOrElse(List(MinifumoErrorWithPath(path.toString, SyntaxError(SourcePos(0,0), "Could not read path"))))

  // Checks a single file for syntax and type errors, including imports.
  def checkFile(path: Path): List[MinifumoErrorWithPath] =
    val idSupply = TypeChecker.IdSupply()
    val io = GlobalSymbolsIo(GlobalSymbolsIo.findProjectRoot(path))
    val globalNames = new ProjectSymbolCache(io, idSupply)
    checkFile(path, globalNames)

  // Checks a file using shared caches to avoid reparsing across a project.
  private def checkFile(path: Path, info: ProjectSymbolCache): List[MinifumoErrorWithPath] =
    val (program, syntaxErrors) = loadProgram(path, info)
    if syntaxErrors.nonEmpty then
      syntaxErrors.map(MinifumoErrorWithPath(path.toString, _))
    else
      val (_, errors) = TypeChecker.checkProgram(info.io.fromPath(path), program, info, true, info.ids)
      if errors.isEmpty then
        Nil
      else
        errors.map(MinifumoErrorWithPath(path.toString, _))

  // Represents an empty program when parsing fails.
  ProgramFile(List(), List())(SourceRange(SourcePos(0, 0), SourcePos(0, 0)))

  // Loads and caches syntax parsing results for a file path.
  private def loadProgram(path: Path, info: ProjectSymbolCache): (ProgramFile, List[SyntaxError]) =
    info.getAst(info.io.makeRelative(path))

  // Renders a source range for error reporting.
  


  // Formats a list of errors for reporting.
  def renderTypeErrors(path: String, errors: List[MinifumoError]): List[String] =
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
      val lines = getLines(Path.of(errorWithPath.p))
      if lineIndex >= 0 && lineIndex < lines.length then
        val sourceLine = lines(lineIndex)
        val startColumn = math.max(1, error.source.start.column)
        val endColumn = math.max(startColumn, error.source.end.column)
        val underlineWidth = math.max(1, endColumn - startColumn + 1)
        val underline = (" " * (startColumn - 1)) + ("^" * underlineWidth)
        s"${errorWithPath.p}:${error.source.start.line}:${error.source.start.column}\n    ${sourceLine}\n    ${underline}\n${error.message}\n\n"
      else
        s"${errorWithPath.p}:${error.source.start.line}:${error.source.start.column}: ${error.message}"
    }

def readLines(path: Path): Vector[String] =
  try
    if path.endsWith("standard.minifumo") then
      Standard.loadStandardSource().lines().toList.asScala.toVector
    else
      Files.readAllLines(path).asScala.toVector
  catch
    case _: Exception =>
      Vector()

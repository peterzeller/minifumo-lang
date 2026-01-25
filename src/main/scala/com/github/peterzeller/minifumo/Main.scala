package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, ProgramFile, SourcePos, SourceRange}
import com.github.peterzeller.minifumo.common.MinifumoError
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseInput}
import com.github.peterzeller.minifumo.typing.TypeChecker
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

object Main:
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

  private def printUsage(): Unit =
    Console.err.println(
      """Usage:
        |  minifumo run <filename>
        |  minifumo check <filename-or-directory>""".stripMargin
    )

  def runFile(path: Path): Either[List[String], Interpreter.Value] =
    val (program, syntaxErrors) = parseProgram(path)
    if syntaxErrors.nonEmpty then
      Left(renderSyntaxErrors(path, syntaxErrors))
    else
      val (typedProgram, typeErrors) = TypeChecker.checkProgram(program)
      if typeErrors.nonEmpty then
        Left(renderTypeErrors(path, typeErrors))
      else
        Right(Interpreter.evalProg(typedProgram, "main"))

  def checkDirectory(path: Path): List[String] =
    if !Files.exists(path) then
      List(s"Directory not found: ${path.toString}")
    else
      Try {
        Using.resource(Files.list(path)) { stream =>
          stream.iterator().asScala.toList
            .filter(Files.isRegularFile(_))
            .filter(_.toString.endsWith(".minifumo"))
            .sortBy(_.toString)
            .flatMap(checkFile)
        }
      }.getOrElse(List(s"Failed reading directory: ${path.toString}"))

  def checkFile(path: Path): List[String] =
    val (program, syntaxErrors) = parseProgram(path)
    if syntaxErrors.nonEmpty then
      renderSyntaxErrors(path, syntaxErrors)
    else
      val (_, errors) = TypeChecker.checkProgram(program)
      if errors.isEmpty then
        Nil
      else
        errors.map(err => s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}")

  private val emptyProgramFile = ProgramFile(List())(SourceRange(SourcePos(0, 0), SourcePos(0, 0)))

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

  private def renderSourceRange(range: com.github.peterzeller.minifumo.ast.SourceRange): String =
    val start = range.start
    val end = range.end
    if start == end then
      s"${start.line}:${start.column}"
    else
      s"${start.line}:${start.column}-${end.line}:${end.column}"

  private def renderSyntaxErrors(path: Path, errors: List[SyntaxError]): List[String] =
    errors.map { err =>
      s"${path.toString}:${err.pos.line}:${err.pos.column}: ${err.message}"
    }

  private def renderTypeErrors(path: Path, errors: List[TypeError]): List[String] =
    errors.map { err =>
      s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}"
    }

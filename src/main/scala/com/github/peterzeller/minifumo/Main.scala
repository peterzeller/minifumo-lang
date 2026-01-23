package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.TypeChecker

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
        val result = runFile(path)
        result match
          case Right(value) =>
            println(value)
          case Left(message) =>
            Console.err.println(message)
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

  def runFile(path: Path): Either[String, Interpreter.Value] =
    parseProgram(path).flatMap { program =>
      val (typedProgram, errors) = TypeChecker.checkProgram(program)
      if errors.nonEmpty then
        errors.foreach(err => Console.err.println(s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}"))
      Right(Interpreter.evalProg(typedProgram, "main"))
    }

  def checkDirectory(path: Path): List[String] =
    if !Files.exists(path) then
      List(s"Path not found: ${path.toString}")
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
    parseProgram(path) match
      case Left(message) =>
        List(message)
      case Right(program) =>
        val (_, errors) = TypeChecker.checkProgram(program)
        if errors.isEmpty then
          Nil
        else
          errors.map(err => s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}")

  private def parseProgram(path: Path): Either[String, com.github.peterzeller.minifumo.ast.ProgramFile] =
    if !Files.exists(path) then
      Left(s"Path not found: ${path.toString}")
    else
      val content = Try {
        Using.resource(scala.io.Source.fromFile(path.toFile))(_.mkString)
      }.toEither.left.map(ex => s"Failed reading ${path.toString}: ${ex.getMessage}")
      content.flatMap { input =>
        Try(AstTransform.program(parseInput(input))).toEither
          .left.map(ex => s"Failed parsing ${path.toString}: ${ex.getMessage}")
      }

  private def renderSourceRange(range: com.github.peterzeller.minifumo.ast.SourceRange): String =
    val start = range.start
    val end = range.end
    if start == end then
      s"${start.line}:${start.column}"
    else
      s"${start.line}:${start.column}-${end.line}:${end.column}"

package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.TypeChecker

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}
import com.github.peterzeller.minifumo.ast.ProgramFile
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.ast.SourcePos
import com.github.peterzeller.minifumo.parser.SyntaxError
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError
import com.github.peterzeller.minifumo.common.MinifumoError
import scala.deriving.Mirror

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

  def runFile(path: Path): (Interpreter.Value, List[MinifumoError]) =
    val (program, syntaxError) = parseProgram(path)
    (Interpreter.evalProg(program, "main"), syntaxError)


  def checkDirectory(path: Path): List[(Path, List[MinifumoError])] =
    if !Files.exists(path) then
      List((path, List(TypeError(s"Directory not found: ${path.toString}", SourceRange(SourcePos(0, 0), SourcePos(0, 0))))))
    else
      Try {
        Using.resource(Files.list(path)) { stream =>
          stream.iterator().asScala.toList
            .filter(Files.isRegularFile(_))
            .filter(_.toString.endsWith(".minifumo"))
            .sortBy(_.toString)
            .flatMap(checkFile)
        }
      } TODO

  def checkFile(path: Path): List[TypeError] =
    let (program, syntaxErrors) = parseProgram(path)

      case Left(message) =>
        List(message)
      case Right(program) =>
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


package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, SourceRange, TopLevel}
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseInput}
import com.github.peterzeller.minifumo.typing.TypeChecker
import munit.FunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ListBuffer

class ExamplesSuite extends FunSuite:
  case class ExpectedError(line: Int, message: String)
  case class ActualError(line: Int, message: String, source: SourceRange)

  val examplesDir: Path = Paths.get("doc/examples")
  val exampleFiles: List[Path] =
    if Files.exists(examplesDir) then
      val stream = Files.list(examplesDir)
      try
        stream.iterator().asScala
          .filter(Files.isRegularFile(_))
          .filter(_.toString.endsWith(".minifumo"))
          .toList
          .sortBy(_.toString)
      finally
        stream.close()
    else
      Nil

  test("check errors in doc/examples"):
    if Files.exists(examplesDir) then
      exampleFiles.foreach(assertExpectedErrors)

  for file <- exampleFiles do
    test(s"run example ${file.getFileName}"):
      val expectedErrors = extractExpectedErrors(Files.readString(file))
      val actualErrors = collectErrors(file)
      if expectedErrors.isEmpty && actualErrors.isEmpty then
        runExample(file)

  def runExample(path: Path): Unit =
    val content = Files.readString(path)

    val (cst, _) = parseInput(content)
    val ast = AstTransform.program(cst)

    val hasMain = ast.items.exists:
      case fun: TopLevel.FunDecl => fun.name == "main"
      case _ => false

    if hasMain then
      val expectedOutputOpt = extractExpectedOutput(content)

      val baos = new ByteArrayOutputStream()
      val ps = new PrintStream(baos)
      val oldOut = System.out
      System.setOut(ps)

      try
        Interpreter.evalProg(ast, "main")
      catch
        case e: Throwable =>
          System.setOut(oldOut)
          throw new RuntimeException(s"Failed to run ${path.getFileName}", e)
      finally
        System.setOut(oldOut)

      val actualOutput = baos.toString().trim.replace("\r\n", "\n")

      expectedOutputOpt.foreach: expected =>
        assertEquals(actualOutput, expected.trim, s"Output mismatch for ${path.getFileName}")

  def assertExpectedErrors(path: Path): Unit =
    val content = Files.readString(path)
    val expectedErrors = extractExpectedErrors(content)
    val actualErrors = collectErrors(path)

    if actualErrors.isEmpty && expectedErrors.isEmpty then
      ()
    else if actualErrors.isEmpty then
      fail(s"Expected errors in ${path.getFileName} but type checker reported none.")
    else if expectedErrors.isEmpty then
      fail(s"Unexpected errors in ${path.getFileName}:\n${formatErrors(actualErrors)}")
    else
      val remainingExpected = ListBuffer.from(expectedErrors)
      actualErrors.foreach { actual =>
        val matchIndex = remainingExpected.indexWhere { expected =>
          expected.line == actual.line && actual.message.contains(expected.message)
        }
        if matchIndex >= 0 then
          remainingExpected.remove(matchIndex)
        else
          fail(
            s"Missing expected error comment in ${path.getFileName} for line ${actual.line}:\n" +
              s"  actual: ${actual.message}"
          )
      }
      if remainingExpected.nonEmpty then
        val extras = remainingExpected.map(err => s"line ${err.line}: ${err.message}").mkString("\n")
        fail(s"Expected error comments without matching errors in ${path.getFileName}:\n$extras")

  def collectErrors(path: Path): List[ActualError] =
    val content = Files.readString(path)
    val (cst, syntaxErrors) = parseInput(content)
    if syntaxErrors.nonEmpty then
      syntaxErrors.map(error => ActualError(error.pos.line, error.message, SourceRange(error.pos, error.pos)))
    else
      val ast = AstTransform.program(cst)
      val (_, typeErrors) = TypeChecker.checkProgram(ast)
      typeErrors.map(error => ActualError(error.source.start.line, error.message, error.source))

  def extractExpectedErrors(content: String): List[ExpectedError] =
    val marker = "// error:"
    content.linesIterator.zipWithIndex.flatMap { case (line, index) =>
      val errors = ListBuffer.empty[ExpectedError]
      var searchFrom = 0
      var found = line.indexOf(marker, searchFrom)
      while found >= 0 do
        val messageStart = found + marker.length
        val nextMarker = line.indexOf(marker, messageStart)
        val rawMessage =
          if nextMarker >= 0 then line.substring(messageStart, nextMarker) else line.substring(messageStart)
        val message = rawMessage.trim
        if message.nonEmpty then
          errors += ExpectedError(index + 1, message)
        searchFrom = if nextMarker >= 0 then nextMarker else line.length
        found = line.indexOf(marker, searchFrom)
      errors.toList
    }.toList

  def formatErrors(errors: List[ActualError]): String =
    errors.map(error => s"line ${error.line}: ${error.message}").mkString("\n")

  def extractExpectedOutput(content: String): Option[String] =
    val lines = content.linesIterator.toList
    val outputMarker = "// Output:"

    lines.indexWhere(_.trim.startsWith(outputMarker)) match
      case -1 => None
      case idx =>
        val line = lines(idx).trim
        val remainder = line.substring(outputMarker.length).trim
        if remainder.nonEmpty then
          Some(remainder)
        else
          val buffer = new StringBuilder
          var i = idx + 1
          while i < lines.length && lines(i).trim.startsWith("//") do
            val commentContent = lines(i).trim.drop(2)
            val cleanContent = if commentContent.startsWith(" ") then commentContent.drop(1) else commentContent
            if buffer.nonEmpty then buffer.append("\n")
            buffer.append(cleanContent)
            i += 1
          Some(buffer.toString())

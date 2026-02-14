package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, SourceRange, TopLevel}
import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath
import com.github.peterzeller.minifumo.parser.parseInput
import munit.FunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ListBuffer

class ExamplesSuite extends FunSuite:
  // Captures an expected error comment with line number and message.
  case class ExpectedError(line: Int, message: String)
  // Captures an actual error with line number, message, and source range.
  case class ActualError(line: Int, message: String, source: SourceRange)

  val examplesDir: Path = Paths.get("doc/examples")
  val ignoreFile: Path = examplesDir.resolve(".minifumoignore")
  // Collects example files under doc/examples, including nested folders.
  val exampleFiles: List[Path] =
    if Files.exists(examplesDir) then
      val allExamples = listExampleFiles()
      if Files.exists(ignoreFile) then
        val ignored = ignoredExampleFiles(allExamples)
        ignored.foreach(file => println(s"WARN: ignoring example ${file.toString}"))
        allExamples.filterNot(ignored.contains)
      else
        allExamples
    else
      Nil

  // Verifies that error expectations match actual errors for all examples.
  test("check errors in doc/examples"):
    if Files.exists(examplesDir) then
      exampleFiles.foreach(assertExpectedErrors)

  for file <- exampleFiles do
    test(s"run example ${file.getFileName}"):
      val content = Files.readString(file)
      val expectedErrors = extractExpectedErrors(content)
      val expectedOutputOpt = extractExpectedOutput(content)
      val hasMain = hasMainFunction(content)
      if expectedErrors.isEmpty && expectedOutputOpt.isEmpty && hasMain then
        fail(s"Example ${file.getFileName} must declare expected output or expected errors.")
      val actualErrors = collectErrors(file)
      if expectedErrors.isEmpty && actualErrors.isEmpty then
        runExample(file)

  // Runs an example via the main entry point, checking for execution errors.
  def runExample(path: Path): Unit =
    val content = Files.readString(path)
    val hasMain = hasMainFunction(content)

    if hasMain then
      val expectedOutputOpt = extractExpectedOutput(content)

      val baos = new ByteArrayOutputStream()
      val ps = new PrintStream(baos)
      val oldOut = System.out
      System.setOut(ps)

      try
        Main.runFile(path) match
          case Left(messages) => fail(s"Type check failed:\n${messages.mkString("\n")}")
          case Right(_) => ()
      catch
        case e: Throwable =>
          System.setOut(oldOut)
          throw new RuntimeException(s"Failed to run ${path.getFileName}", e)
      finally
        System.setOut(oldOut)

      val actualOutput = baos.toString().trim.replace("\r\n", "\n")

      expectedOutputOpt.foreach: expected =>
        assertEquals(actualOutput, expected.trim, s"Output mismatch for ${path.getFileName}")

  // Asserts that error comments in the file match actual errors.
  def assertExpectedErrors(path: Path): Unit =
    val content = Files.readString(path)
    val expectedErrors = extractExpectedErrors(content)
    val actualErrors = collectErrors(path)

    if actualErrors.isEmpty && expectedErrors.isEmpty then
      ()
    else if actualErrors.isEmpty then
      fail(s"Expected errors in ${path.getFileName} but type checker reported none.")
    else if expectedErrors.isEmpty then
      fail(s"Unexpected errors in ${path.getFileName}:\n${Main.renderTypeErrors(actualErrors)}")
    else
      val remainingExpected = ListBuffer.from(expectedErrors)
      actualErrors.foreach { actual =>
        val matchIndex = remainingExpected.indexWhere { expected =>
          expected.line == actual.err.source.start.line && actual.err.message.contains(expected.message)
        }
        if matchIndex >= 0 then
          remainingExpected.remove(matchIndex)
        else
          fail(
            s"Missing expected error comment in ${path.getFileName} for line ${actual.err.source.start.line}:\n" +
              s"  actual: ${actual.err.message}"
          )
      }
      if remainingExpected.nonEmpty then
        val extras = remainingExpected.map(err => s"line ${err.line}: ${err.message}").mkString("\n")
        fail(s"Expected error comments without matching errors in ${path.getFileName}:\n$extras")

  // Collects errors using the CLI entry point to include import resolution.
  def collectErrors(path: Path): List[MinifumoErrorWithPath] =
    Main.checkFile(path)

  // Extracts expected errors from source comments.
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



  // Extracts expected output comments from the example source.
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

  // Determines whether a file declares a top-level main function.
  private def hasMainFunction(content: String): Boolean =
    val (cst, _) = parseInput(content)
    val ast = AstTransform.program(cst)
    ast.items.exists:
      case fun: TopLevel.FunDecl => fun.sig.name == "main"
      case _ => false


  // Lists all example files in the examples directory.
  private def listExampleFiles(): List[Path] =
    val stream = Files.walk(examplesDir)
    try
      stream.iterator().asScala
        .filter(Files.isRegularFile(_))
        .filter(_.toString.endsWith(".minifumo"))
        .toList
        .sortBy(_.toString)
    finally
      stream.close()

  // Determines which example files are ignored based on the ignore list.
  private def ignoredExampleFiles(allExamples: List[Path]): List[Path] =
    val patterns = Files.readAllLines(ignoreFile).asScala
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList
    val matchers = patterns.map(pattern => examplesDir.getFileSystem.getPathMatcher(s"glob:${pattern}"))
    allExamples.filter { file =>
      val relative = examplesDir.relativize(file)
      matchers.exists(_.matches(relative))
    }

package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, TopLevel}
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.parseInput
import munit.FunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class ExamplesSuite extends FunSuite:

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

  test("type check all files in doc/examples"):
    if Files.exists(examplesDir) then
      val errors = Main.checkDirectory(examplesDir)
      if errors.nonEmpty then
        fail(s"Type check failed:\n${errors.mkString("\n")}")

  for file <- exampleFiles do
    test(s"run example ${file.getFileName}"):
      runExample(file)

  def runExample(path: Path): Unit =
    val content = Files.readString(path)

    val cst = parseInput(content)
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

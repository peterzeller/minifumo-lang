package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.TopLevel
import com.github.peterzeller.minifumo.backends.scala.ScalaBackend
import com.github.peterzeller.minifumo.typing.GlobalSymbolsIo
import com.github.peterzeller.minifumo.parser.parseInput
import munit.FunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

class ScalaBackendSuite extends FunSuite:

  /** Detects whether the scala command-line runner is available. */
  private def scalaAvailable: Boolean =
    Try(Process(Seq("scala", "-version")).! == 0).getOrElse(false)

  /** Detects whether the scalac compiler command is available. */
  private def scalacAvailable: Boolean =
    Try(Process(Seq("scalac", "-version")).! == 0).getOrElse(false)

  /** Lists all example files recursively from doc/examples. */
  private def listExampleFiles(examplesDir: Path): List[Path] =
    val stream = Files.walk(examplesDir)
    try
      stream.iterator().asScala
        .filter(Files.isRegularFile(_))
        .filter(_.toString.endsWith(".minifumo"))
        .toList
        .sortBy(_.toString)
    finally
      stream.close()

  /** Extracts expected output comments from one example source file. */
  private def extractExpectedOutput(content: String): Option[String] =
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

  /** Determines whether a file defines a top-level main function. */
  private def hasMainFunction(content: String): Boolean =
    val (ast, _) = parseInput(content)
    ast.items.exists:
      case fun: TopLevel.FunDecl => fun.sig.name == "main"
      case _ => false

  /** Runs a shell command and returns exit code plus captured output. */
  private def runProcess(command: Seq[String], cwd: Path): (Int, String) =
    val output = new StringBuilder
    val logger = ProcessLogger(
      line => { output.append(line).append("\n"); () },
      line => { output.append(line).append("\n"); () }
    )
    val code = Process(command, cwd.toFile).!(logger)
    (code, output.toString())

  /** Captures interpreter output from running a Minifumo example through Main.runFile. */
  private def runMinifumoExample(file: Path): String =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val oldOut = System.out
    System.setOut(ps)
    try
      Main.runFile(file) match
        case Left(errors) => fail(s"Minifumo run failed for ${file}:\n${Main.renderTypeErrors(errors).mkString("\n")}")
        case Right(_) => ()
    finally
      System.setOut(oldOut)
    baos.toString().trim.replace("\r\n", "\n")

  /** Verifies that well-typed examples can be translated to Scala wrappers as a directory run. */
  test("compileToScala translates all well-typed examples in folder mode"):
    val examplesDir = Paths.get("doc/examples")
    val tempRoot = Files.createTempDirectory("mf-scala-welltyped")
    Files.writeString(tempRoot.resolve("minifumo.yml"), "name: scala-backend-test\n")
    val allExamples = listExampleFiles(examplesDir)
    val wellTyped = allExamples.filter(file => Main.checkFile(file).isEmpty)
    wellTyped.foreach: file =>
      val relative = examplesDir.relativize(file)
      val target = tempRoot.resolve(relative)
      Option(target.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(target, Files.readString(file))
    val outputDir = Files.createTempDirectory("mf-scala-folder")
    val result = ScalaBackend.compile(tempRoot, outputDir)
    assert(result.isRight, result.left.toOption.map(Main.renderTypeErrors).getOrElse(Nil).mkString("\n"))
    assertEquals(result.toOption.get.files.length, wellTyped.length)

  /** Verifies generated wrappers compile and produce equivalent outputs in the Scala interpreter. */
  test("compileToScala file mode generates runnable Scala wrappers for output examples"):
    assume(scalaAvailable && scalacAvailable, "scala/scalac are not installed in this environment")
    val examplesDir = Paths.get("doc/examples")
    val root = GlobalSymbolsIo.findProjectRoot(examplesDir)
    val outputRoot = Files.createTempDirectory("mf-scala-files")
    val classesDir = Files.createTempDirectory("mf-scala-classes")
    val runtimeClassPath = System.getProperty("java.class.path")
    val candidates = listExampleFiles(examplesDir).filter: file =>
      val content = Files.readString(file)
      Main.checkFile(file).isEmpty && hasMainFunction(content) && extractExpectedOutput(content).nonEmpty
    assert(candidates.nonEmpty, "Expected at least one runnable example with Output comments")

    val failures = candidates.flatMap: file =>
      val relative = root.relativize(file).toString.replace('\\', '/')
      val compiled = ScalaBackend.compile(file, outputRoot)
      compiled.left.toOption.map(errors => s"Failed to compile ${file}:\n${Main.renderTypeErrors(errors).mkString("\n")}") match
        case Some(err) => Some(err)
        case None =>
          val generatedFile = outputRoot.resolve(relative.stripSuffix(".minifumo") + ".scala")
          val compileCmd = Seq("scalac", "-classpath", runtimeClassPath, "-d", classesDir.toString, generatedFile.toString)
          val (compileCode, compileOutput) = runProcess(compileCmd, Paths.get("."))
          if compileCode != 0 then
            Some(s"Failed scalac for ${generatedFile}:\n${compileOutput}")
          else
            val moduleRef = ScalaBackend.moduleRefFor(relative)
            val runCp = s"${classesDir}:${runtimeClassPath}"
            val runCmd = Seq("scala", "-classpath", runCp, moduleRef.qualifiedName)
            val (runCode, runOutput) = runProcess(runCmd, root)
            val expectedOutput = extractExpectedOutput(Files.readString(file)).get.trim
            val actualMinifumo = runMinifumoExample(file)
            val actualScala = runOutput.trim.replace("\r\n", "\n")
            if runCode != 0 then
              Some(s"Failed scala run for ${moduleRef.qualifiedName}:\n${runOutput}")
            else if actualScala != expectedOutput then
              Some(s"Unexpected Scala output for ${file}: expected '${expectedOutput}', got '${actualScala}'")
            else if actualScala != actualMinifumo then
              Some(s"Scala output differs from Main.runFile output for ${file}: '${actualScala}' vs '${actualMinifumo}'")
            else
              None
    assertEquals(failures, Nil)

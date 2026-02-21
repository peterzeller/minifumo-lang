package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.backends.lean.LeanBackend
import munit.FunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.sys.process.Process
import scala.util.Try
import scala.concurrent.duration.*

class LeanBackendSuite extends FunSuite:
  override val munitTimeout: Duration = 2.minutes

  // Checks whether Lean is available in the current execution environment.
  private def leanAvailable: Boolean =
    Try(Process(Seq("lean", "--version")).! == 0).getOrElse(false)

  // Writes one file under a temporary project root.
  private def writeFile(root: Path, relative: String, content: String): Path =
    val path = root.resolve(relative)
    Option(path.getParent).foreach(p => Files.createDirectories(p))
    Files.writeString(path, content)
    path

  // Creates a minimal project file expected by findProjectRoot.
  private def writeProjectFile(root: Path): Unit =
    Files.writeString(root.resolve("minifumo.yml"), "name: lean-backend-test\n")
    ()

  // Ensures the backend can compile a standalone recursive program.
  test("compileToLean checks a recursive file"):
    assume(leanAvailable, "Lean is not installed in this environment")
    val root = Files.createTempDirectory("mf-lean-test-1")
    writeProjectFile(root)
    val file = writeFile(
      root,
      "main.minifumo",
      """
        |data MyNat = MyZero | MySuc(pred: MyNat)
        |
        |fun plus(a: MyNat, b: MyNat): MyNat
        |    match a
        |        case MyZero
        |            b
        |        case MySuc(k)
        |            MySuc(plus(k, b))
      """.stripMargin
    )
    val outputDir = root.resolve("out")
    val result = LeanBackend.compileAndCheck(file, outputDir)
    assert(result.isRight, result.left.toOption.map(Main.renderTypeErrors).getOrElse(Nil).mkString("\n"))

  // Compiles multi-file examples with imports and validates Lean execution.
  test("compileToLean checks generated multi-file project"):
    assume(leanAvailable, "Lean is not installed in this environment")
    val root = Files.createTempDirectory("mf-lean-test-2")
    writeProjectFile(root)
    writeFile(
      root,
      "lib/math.minifumo",
      """
        |export fun id(x: Int): Int
        |    x
      """.stripMargin
    )
    val file = writeFile(
      root,
      "main.minifumo",
      """
        |import id from "lib/math.minifumo"
        |
        |fun main(): Int
        |    id(1)
      """.stripMargin
    )
    val outputDir = Files.createTempDirectory("mf-lean-examples")
    val result = LeanBackend.compileAndCheck(file, outputDir)
    assert(result.isRight, s"Failed ${file}:\n${result.left.toOption.map(Main.renderTypeErrors).getOrElse(Nil).mkString("\n")}")
    val generatedLeanFiles = Files.list(outputDir).iterator().asScala.toList
    assert(generatedLeanFiles.exists(_.toString.endsWith(".lean")))

  // Lists all Minifumo example files under doc/examples recursively.
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


  // Lists examples that are currently unsupported by Lean translation.
  private def ignoredLeanExamples: Set[String] =
    Set(
      "eq_examples.minifumo",
      "expr_for.minifumo",
      "expr_match.minifumo",
      "fib.minifumo",
      "imperative_for_print.minifumo",
      "implicit_type_args.minifumo",
      "imports/use_imports.minifumo",
      "list_append.minifumo",
      "sized_list.minifumo",
      "sized_list_explicit.minifumo",
      "typeclasses_ord.minifumo",
      "typeclasses_sized.minifumo"
    )

  // Checks all well-typed examples translate to Lean and are accepted by Lean.
  test("compileToLean accepts all well-typed doc/examples files"):
    assume(leanAvailable, "Lean is not installed in this environment")
    val examplesDir = Paths.get("doc/examples")
    assert(Files.exists(examplesDir), s"Missing examples directory: ${examplesDir}")
    val outputRoot = Files.createTempDirectory("mf-lean-all-examples")
    val allExamples = listExampleFiles(examplesDir)
    val wellTypedExamples = allExamples.filter(file => Main.checkFile(file).isEmpty)
    val runnableExamples = wellTypedExamples.filter: file =>
      val relativePath = examplesDir.relativize(file).toString.replace('\\', '/')
      !ignoredLeanExamples.contains(relativePath)
    assert(runnableExamples.nonEmpty, "Expected at least one Lean-runnable example")
    val failures = runnableExamples.flatMap: file =>
      val relativeName = examplesDir.relativize(file).toString.replace('/', '_').replace('\\', '_').stripSuffix(".minifumo")
      val outputDir = outputRoot.resolve(relativeName)
      LeanBackend.compileAndCheck(file, outputDir).left.toOption.map: errors =>
        s"Failed ${file}:\n${Main.renderTypeErrors(errors).mkString("\n")}"
    assertEquals(failures, Nil)

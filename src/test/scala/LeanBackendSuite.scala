package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.backends.lean.LeanBackend
import munit.FunSuite

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.sys.process.Process
import scala.util.Try

class LeanBackendSuite extends FunSuite:
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


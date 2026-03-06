package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, ProjectSymbolCache, TypeChecker}
import munit.FunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class TutorialExamplesSuite extends FunSuite:
  private val tutorialExamplesDir: Path = Paths.get("doc/tutorial/examples")

  // Lists tutorial example files so each can be checked in a separate test case.
  private def tutorialExampleFiles(): List[Path] =
    if !Files.exists(tutorialExamplesDir) then
      Nil
    else
      val fileStream = Files.walk(tutorialExamplesDir)
      try
        fileStream.iterator().asScala
          .filter(Files.isRegularFile(_))
          .filter(_.toString.endsWith(".minifumo"))
          .toList
          .sortBy(_.toString)
          .map(_.toAbsolutePath.normalize())
      finally
        fileStream.close()

  // Creates a symbol cache rooted at the repository for tutorial-file type checking.
  private def makeSymbolCache(idSupply: TypeChecker.IdSupply): ProjectSymbolCache =
    val repositoryRoot = Paths.get(".").toAbsolutePath.normalize()
    new ProjectSymbolCache(GlobalSymbolsIo(repositoryRoot), idSupply)

  // Ensures each tutorial snippet type-checks in CI.
  for file <- tutorialExampleFiles() do
    test(s"tutorial example type-checks: ${file.getFileName}"):
      val idSupply = TypeChecker.IdSupply()
      val symbolCache = makeSymbolCache(idSupply)
      val relativePath = symbolCache.io.makeRelative(file)
      val (program, syntaxErrors) = symbolCache.getAst(relativePath)
      assertEquals(syntaxErrors, Nil)
      val (_, typeErrors) = TypeChecker.checkProgram(file.toString, program, symbolCache, true, idSupply)
      assertEquals(typeErrors, Nil)

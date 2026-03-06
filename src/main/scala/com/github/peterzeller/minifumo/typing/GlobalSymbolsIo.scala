package com.github.peterzeller.minifumo.typing

import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec

class GlobalSymbolsIo(projectRoot: Path) {

  def fromPath(p: Path): String =
    projectRoot.relativize(p).normalize().toString

  def makeRelative(path: Path): String =
    fromPath(path)

  def toPath(importPath: String): Path =
    if importPath.endsWith("standard.minifumo") then
      return Paths.get("standard.minifumo")
    projectRoot.resolve(importPath)

  def readInput(s: String): String =
    Files.readString(toPath(s))
}

object GlobalSymbolsIo:
  def create(path: String): GlobalSymbolsIo =
    GlobalSymbolsIo(findProjectRoot(Path.of(path)))

  // find the folder that contains minifumo.yml
  @tailrec
  def findProjectRoot(path: Path): Path =
    if path.toFile.isDirectory then
      if path.resolve("minifumo.yml").toFile.exists() then
        return path
    val parent = path.getParent
    if parent == path then
      throw new RuntimeException("could not minifumo project root")
    findProjectRoot(path.getParent)
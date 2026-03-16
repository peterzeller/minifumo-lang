package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.SourcePos
import com.github.peterzeller.minifumo.parser.parseInput

import java.nio.file.Path

class HoverLookupSuite extends munit.FunSuite:
  private val testFile = "/test/input.minifumo"

  test("hoverAt returns type and top-level comment") {
    val sourceWithCursor =
      """// returns the input
        |fun target(n: Int): Int
        |  n
        |
        |fun main(): Int
        |  ta|rget(1)
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)

    val hover = HoverLookup.hoverAt(typedProgram, cursor, testFile)
    assert(hover.nonEmpty)
    assert(hover.get.typeText.contains("Int"))
  }

  test("hoverAt returns let-binder comment") {
    val sourceWithCursor =
      """fun main(): Int
        |  // local value
        |  let x: Int = 1 in x|
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)

    val hover = HoverLookup.hoverAt(typedProgram, cursor, testFile)
    assert(hover.nonEmpty)
  }

  test("hoverAt works in let binding") {
    val sourceWithCursor =
      """// returns the input
        |fun target(n: Int): Int
        |  n
        |
        |fun main(): Int
        |  let x = 5
        |  ta|rget(x)
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)

    val hover = HoverLookup.hoverAt(typedProgram, cursor, testFile)
    assert(hover.nonEmpty)
    assert(hover.get.typeText.contains("Int"), s"hover is $hover")
  }

  private def checkTyped(source: String) =
    val ids = TypeChecker.IdSupply()
    val symbolCache = ProjectSymbolCache(new GlobalSymbolsIo(Path.of(".")), ids)
    symbolCache.addInput(testFile, source)
    val (program, syntaxErrors) = parseInput(source)
    assertEquals(syntaxErrors, Nil)
    val (typedProgram, typeErrors) = TypeChecker.checkProgram(testFile, program, symbolCache, importStandard = true, ids)
    assertEquals(typeErrors, Nil)
    typedProgram

  private def extractCursor(sourceWithCursor: String): (String, SourcePos) =
    val markerIndex = sourceWithCursor.indexOf('|')
    assert(markerIndex >= 0, "Missing cursor marker '|'.")
    val source = sourceWithCursor.substring(0, markerIndex) + sourceWithCursor.substring(markerIndex + 1)
    (source, offsetToPos(sourceWithCursor, markerIndex))

  private def offsetToPos(source: String, offset: Int): SourcePos =
    val before = source.substring(0, offset)
    val lines = before.split("\\n", -1)
    SourcePos(lines.length, lines.last.length + 1)

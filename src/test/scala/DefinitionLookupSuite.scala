package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.SourcePos
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.TypedAst.Expr
import com.github.peterzeller.minifumo.typing.TypedAst.Pattern
import java.nio.file.Path

class DefinitionLookupSuite extends munit.FunSuite:
  private val testFile = "/test/input.minifumo"

  test("definitionAt resolves global function references from cursor fixture") {
    val sourceWithCursor =
      """fun target(n: Int): Int
        |  n
        |
        |fun main(): Int
        |  ta|rget(1)
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)
    val targetPos = posOfFirst(source, "target")

    val definition = DefinitionLookup.definitionAt(typedProgram, cursor, testFile)
    assert(definition.nonEmpty)
    assertEquals(definition.get.file, testFile)
    assert(definition.get.range.contains(targetPos))

    val node = DefinitionLookup.findElementAtCursor(typedProgram, cursor)
    assert(node.nonEmpty)
    node.get match
      case Expr.Var(_: FunctionSymbol) => ()
      case other => fail(s"Expected function variable node, got $other")
  }

  test("definitionAt resolves local symbols from cursor fixture") {
    val sourceWithCursor =
      """fun main(x: Int): Int
        |  x|
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)
    val paramPos = posOfFirst(source, "x: Int")

    val definition = DefinitionLookup.definitionAt(typedProgram, cursor, testFile)
    assert(definition.nonEmpty)
    assertEquals(definition.get.file, testFile)
    assert(definition.get.range.contains(paramPos))

    val node = DefinitionLookup.findElementAtCursor(typedProgram, SourcePos(cursor.line, cursor.column - 1))
    assert(node.nonEmpty)
    node.get match
      case Expr.Var(_: LocalSymbol) => ()
      case other => fail(s"Expected local variable node, got $other")
  }


  test("definitionAt resolves receiver symbol in field access expressions") {
    val sourceWithCursor =
      """data Box = MkBox(value: Int)
        |
        |fun main(foo: Box): Int
        |  fo|o.value
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)
    val paramPos = posOfFirst(source, "foo: Box")

    val definition = DefinitionLookup.definitionAt(typedProgram, cursor, testFile)
    assert(definition.nonEmpty)
    assertEquals(definition.get.file, testFile)
    assert(definition.get.range.contains(paramPos))

    val node = DefinitionLookup.findElementAtCursor(typedProgram, cursor)
    assert(node.nonEmpty)
    node.get match
      case Expr.Var(_: LocalSymbol) => ()
      case other => fail(s"Expected local variable node for field access receiver, got $other")
  }

  test("definitionAt resolves field accessor symbol in field access expressions") {
    val sourceWithCursor =
      """data Box = MkBox(value: Int)
        |
        |fun main(foo: Box): Int
        |  foo.va|lue
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)
    val accessorPos = posOfFirst(source, "value: Int")

    val definition = DefinitionLookup.definitionAt(typedProgram, cursor, testFile)
    assert(definition.nonEmpty)
    assertEquals(definition.get.file, testFile)
    assert(definition.get.range.contains(accessorPos))

    val node = DefinitionLookup.findElementAtCursor(typedProgram, cursor)
    assert(node.nonEmpty)
    node.get match
      case Expr.App(Expr.Var(symbol: FunctionSymbol), _, _) =>
        assertEquals(symbol.name, "Box_value")
      case Expr.Var(symbol: FunctionSymbol) =>
        assertEquals(symbol.name, "Box_value")
      case other => fail(s"Expected generated field accessor function node, got $other")
  }
  test("definitionAt resolves constructor references in patterns from cursor fixture") {
    val sourceWithCursor =
      """data Box = MkBox(value: Int)
        |
        |fun unwrap(b: Box): Int
        |  match b
        |    case Mk|Box(v)
        |      v
        |""".stripMargin
    val (source, cursor) = extractCursor(sourceWithCursor)
    val typedProgram = checkTyped(source)
    val ctorPos = posOfFirst(source, "MkBox(value")
    val node = DefinitionLookup.findElementAtCursor(typedProgram, cursor)
    assert(node.nonEmpty)
    node.get match
      case Pattern.Ctor(symbol, _) =>
        assertEquals(symbol.name, "MkBox")
      case other =>
        fail(s"Expected constructor pattern node before definition lookup, got $other")

    val definition = DefinitionLookup.definitionAt(typedProgram, cursor, testFile)
    assert(definition.nonEmpty, s"Expected definition for cursor $cursor, node at cursor: $node")
    assertEquals(definition.get.file, testFile)
    assert(definition.get.range.contains(ctorPos))
  }

  /** Parses, type-checks, and returns a typed program with no errors. */
  private def checkTyped(source: String) =
    val ids = TypeChecker.IdSupply()
    val symbolCache = ProjectSymbolCache(new GlobalSymbolsIo(Path.of(".")), ids)
    symbolCache.addInput(testFile, source)
    val (program, syntaxErrors) = parseInput(source)
    assertEquals(syntaxErrors, Nil)
    val (typedProgram, typeErrors) = TypeChecker.checkProgram(testFile, program, symbolCache, importStandard = true, ids)
    assertEquals(typeErrors, Nil)
    typedProgram

  /** Removes one cursor marker and returns the cleaned source and 1-based cursor position. */
  private def extractCursor(sourceWithCursor: String): (String, SourcePos) =
    val markerIndex = sourceWithCursor.indexOf('|')
    assert(markerIndex >= 0, "Missing cursor marker '|'.")
    val secondMarker = sourceWithCursor.indexOf('|', markerIndex + 1)
    assertEquals(secondMarker, -1, "Expected exactly one cursor marker '|'.")
    val source = sourceWithCursor.substring(0, markerIndex) + sourceWithCursor.substring(markerIndex + 1)
    (source, offsetToPos(sourceWithCursor, markerIndex))

  /** Converts a zero-based offset into a 1-based SourcePos. */
  private def offsetToPos(source: String, offset: Int): SourcePos =
    val before = source.substring(0, offset)
    val lines = before.split("\n", -1)
    val line = lines.length
    val column = lines.last.length + 1
    SourcePos(line, column)

  /** Returns the source position of the first occurrence of a needle string. */
  private def posOfFirst(source: String, needle: String): SourcePos =
    val index = source.indexOf(needle)
    assert(index >= 0, s"Could not find '$needle' in test source.")
    offsetToPos(source, index)

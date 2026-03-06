package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.checkProgram
import com.github.peterzeller.minifumo.parser.parseInput


class GlobalSymbolsSuite extends munit.FunSuite:
  // Parses a program from a string for global symbol tests.
  private def parseProgram(input: String): ast.ProgramFile =
    val (ast, _) = parseInput(input)
    ast

  case class DummyNameCache(names: Map[String, Map[String, GlobalName]]) extends NameCache with SymbolCache:
    override def globalNames(path: String): Map[String, GlobalName] = names.getOrElse(path, Map())
    override def globalSymbols(path: String): Map[String, GlobalSymbol] = ???
    override def globalEnv(path: String): TypeChecker.GlobalEnv = ???

  private val ids = TypeChecker.IdSupply()

  test("buildGlobalNames respects export flags") {
    val program = parseProgram("""
      |export data Foo = MakeFoo
      |data Hidden = MakeHidden
      |export fun visible(): Int
      |  1
      |fun hidden(): Int
      |  2
    """.stripMargin)
    val names = GlobalSymbols.buildGlobalNames("main.minifumo", program, onlyExported = true)
    assertEquals(names.keySet, Set("Foo", "MakeFoo", "visible"))
  }


//  test("buildGlobalSymbols reports undefined types in signatures") {
//    val program = parseProgram("""
//      |fun bad(x: Missing): Int
//      |  1
//    """.stripMargin)
//    val cache = DummyNameCache(Map())
//    val (_, errors) = GlobalSymbols.buildGlobalSymbols("main.minifumo", program, cache, onlyExported = false, ids)
//    assert(errors.exists(_.message.contains("Could not find Missing")), s"errors = ${errors.mkString("\n")}")
//  }

  test("resolveImports uses the name cache for imported symbols") {
    val program = ast.ProgramFile(
      imports = List(ast.ImportStatement("foo", Some("lib"), None)(ast.SourceRange.empty)),
      items = List()
    )(ast.SourceRange.empty)
    val cache = DummyNameCache(
      Map("lib" -> Map("foo" -> GlobalName("lib", "foo")))
    )
    val (imports, errors) = GlobalSymbols.resolveImports(program, cache)
    assert(errors.isEmpty)
    assertEquals(imports.keySet, Set("foo"))
  }

  test("parser supports explicit constructor result types") {
    val (program, parseErrors) = parseInput("""
      |data List[T: Type] =
      |   Nil: List[T]
      | | Cons(head: T, tail: List[T]): List[T]
    """.stripMargin)
    assertEquals(parseErrors, List())
    val dataDecl = program.items.collectFirst { case d: ast.TopLevel.DataDecl => d }.getOrElse(fail("Expected a data declaration"))
    assert(dataDecl.ctors.forall(_.returnType.nonEmpty))
  }

  test("supports sized list signatures with explicit constructor result types") {
    val (program, parseErrors) = parseInput("""
      |export data SizedList[T: Type, N: Nat] =
      |   SizedNil: SizedList[T, 0]
      | | SizedCons(head: T, tail: SizedList[T, N]): SizedList[T, Suc(N)]
      |
      |export fun appendSizedList[T: Type, N: Nat, M: Nat](xs: SizedList[T, N], ys: SizedList[T, M]): SizedList[T, natAdd(N, M)]
      |  xs
    """.stripMargin)
    assertEquals(parseErrors, List())
    val cache = DummyNameCache(Map())
    val (symbols, errors) = GlobalSymbols.buildGlobalSymbols("main.minifumo", program, cache, onlyExported = false, ids)
    assertEquals(errors, List())
    assert(symbols.contains("SizedList"))
    assert(symbols.contains("SizedNil"))
    assert(symbols.contains("SizedCons"))
    assert(symbols.contains("appendSizedList"))
  }

  test("user signatures can reference dependent Eq indices from standard library") {
    val input = """
      |export fun eqExampleF(x: Int, y: Int, z: Int): Int
      |  opPlus(opPlus(x, y), z)
      |
      |export fun example(x: Int, eq: Eq[Int, x, 4]): Eq[Int, eqExampleF(x, 3, 4), eqExampleF(4, 3, 4)]
      |  congrArg[Int, Int, x, 4]((n) => eqExampleF(n, 3, 4), eq)
    """.stripMargin
    val (program, parseErrors) = parseInput(input)
    assertEquals(parseErrors, List())

    val dummyPath = "eq_test.minifumo"
    val cache = ProjectSymbolCache(GlobalSymbolsIo.create("."), ids)
    cache.addInput(dummyPath, input)
    val (_, errors) = checkProgram(dummyPath, program, cache, importStandard = true, ids)
    assertEquals(errors, List())
  }

package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.parser.parseInput

import java.nio.file.Paths

class GlobalSymbolsSuite extends munit.FunSuite:
  // Parses a program from a string for global symbol tests.
  private def parseProgram(input: String): ast.ProgramFile =
    val (cst, _) = parseInput(input)
    AstTransform.program(cst)

  // Builds a name cache from a fixed map for import resolution tests.
  private def fixedNameCache(names: Map[String, Map[String, GlobalName]]): NameCache =
    new NameCache:
      override def globalNames(path: String): Map[String, GlobalName] = names.getOrElse(path, Map())

  test("buildGlobalNames respects export flags") {
    val program = parseProgram("""
      |export data Foo = Foo
      |data Hidden = Hidden
      |export fun visible(): Int
      |  1
      |fun hidden(): Int
      |  2
    """.stripMargin)
    val names = GlobalSymbols.buildGlobalNames(Paths.get("main.minifumo"), program, onlyExported = true)
    assertEquals(names.keySet, Set("Foo", "visible"))
  }

  test("checkSignatureExpr resolves known names and rejects lambdas") {
    val source = ast.SourceRange.empty
    val env = GlobalSymbols.PreEnv(globalNames = Map("Int" -> GlobalName(Paths.get("std"), "Int")))
    val intExpr = ast.Expr.Var("Int")(source)
    val (resolved, errors) = GlobalSymbols.checkSignatureExpr(intExpr, env)
    assert(errors.isEmpty)
    assert(resolved.isInstanceOf[TypedAst.Expr.Var])
    val lambdaParam = ast.LambdaParam("x", None)(source)
    val lambdaExpr = ast.Expr.Lambda(lambdaParam, ast.Expr.Var("x")(source))(source)
    val (_, lambdaErrors) = GlobalSymbols.checkSignatureExpr(lambdaExpr, env)
    assert(lambdaErrors.nonEmpty)
  }

  test("buildGlobalSymbols reports undefined types in signatures") {
    val program = parseProgram("""
      |fun bad(x: Missing): Int
      |  1
    """.stripMargin)
    val cache = fixedNameCache(Map())
    val (_, errors) = GlobalSymbols.buildGlobalSymbols(Paths.get("main.minifumo"), program, cache, onlyExported = false)
    assert(errors.exists(_.message.contains("Could not find Missing")))
  }

  test("resolveImports uses the name cache for imported symbols") {
    val program = parseProgram("""
      |import foo from "lib"
      |fun main(): Int
      |  1
    """.stripMargin)
    val cache = fixedNameCache(
      Map("lib" -> Map("foo" -> GlobalName(Paths.get("lib"), "foo")))
    )
    val (imports, errors) = GlobalSymbols.resolveImports(program, cache)
    assert(errors.isEmpty)
    assertEquals(imports.keySet, Set("foo"))
  }

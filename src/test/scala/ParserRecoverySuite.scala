import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, ProjectSymbolCache, TypeChecker}

class ParserRecoverySuite extends munit.FunSuite {
  // Builds a symbol cache and type-checks an input program.
  private def typeCheck(input: String): List[TypeChecker.TypeError] =
    val path = "parser-recovery.minifumo"
    val cache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    cache.addInput(path, input)
    val (program, _) = parseInput(input)
    val (_, errors) = TypeChecker.checkProgram(path, program, cache, true, cache.ids)
    errors

  test("parser recovers at next fun declaration") {
    val input =
      """
        |fun bad(): Int
        |    let x =
        |
        |fun good(): Int
        |    1
      """.stripMargin
    val (program, parseErrors) = parseInput(input)
    assert(parseErrors.nonEmpty)
    val names = program.items.collect { case f: ast.TopLevel.FunDecl => f.sig.name }
    assertEquals(names, List("bad", "good"))
  }

  test("parser inserts hole when suite is missing at end of input") {
    val input =
      """
        |fun unfinished(): Int
      """.stripMargin
    val (program, _) = parseInput(input)
    val fun = program.items.collectFirst { case f: ast.TopLevel.FunDecl if f.sig.name == "unfinished" => f }
      .getOrElse(fail("Expected function declaration"))
    assert(fun.body.isInstanceOf[ast.Expr.Hole])
  }

  test("lambda allows suite body after fat arrow") {
    val input =
      """
        |fun id(): Int -> Int
        |    (x: Int) =>
        |        x
      """.stripMargin
    val (program, parseErrors) = parseInput(input)
    assertEquals(parseErrors, Nil)
    val fun = program.items.collectFirst { case f: ast.TopLevel.FunDecl if f.sig.name == "id" => f }
      .getOrElse(fail("Expected function declaration"))
    assert(fun.body.isInstanceOf[ast.Expr.Lambda])
  }



  test("hole in let body reports propagated expected type") {
    val input =
      """
        |fun missingViaLet(): Int
        |    let x = 1
        |    ???
      """.stripMargin
    val errors = typeCheck(input)
    assert(errors.exists(_.message.contains("Int is expected for this hole")))
  }

  test("hole token ??? parses and reports expected type when checked") {
    val input =
      """
        |fun missing(): Int
        |    ???
      """.stripMargin
    val errors = typeCheck(input)
    assert(errors.exists(_.message.contains("Int is expected")))
  }
}

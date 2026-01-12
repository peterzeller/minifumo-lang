// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.ast.AstTransform
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val c = parseInput("""
      |fun add(x Int, y Int) Int
      |    x + y
    """.stripMargin)
    println("CST: " + c.toStringTree(new MinifumoParser(null)))
    val ast = AstTransform.program(c)
    println("AST: " + ast)
  }
}

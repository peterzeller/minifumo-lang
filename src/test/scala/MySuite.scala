// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.antlr.MinifumoParser
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val c = parseInput("""
      |fun add(x Int, y Int) Int
      |    x + y
    """.stripMargin)
    println(c.toStringTree(new MinifumoParser(null)))
  }
}

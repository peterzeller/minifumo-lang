// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.typing.TypeChecker
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

  test("interpreter evals main with 1+2") {
    val c = parseInput("""
      |fun main() Int
      |    1 + 2
    """.stripMargin)
    val ast = AstTransform.program(c)
    val result = Interpreter.evalProg(ast, "main")
    assertEquals(result, Interpreter.Value.IntVal(BigInt(3)))
  }

  test("type checker can type ints") {
    val c = parseInput("""
      |fun main() Int
      |    let x = 1
      |    let y = 2
      |    x + y
    """.stripMargin)
    val ast = AstTransform.program(c)
    val result = Interpreter.evalProg(ast, "main")
    assertEquals(result, Interpreter.Value.IntVal(BigInt(3)))
    val (typed, errors) = TypeChecker.checkProgram(ast)
    println(s"Found ${errors.length} type errors.")
    for err <- errors do
      println("Type error: " + err)
    println("Typed AST: " + typed)
  }
}

// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.typing.TypeChecker
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  test("interpreter evals main with 1+2") {
    val c = parseInput("""
      |fun main() Int
      |    1 + 2
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typedProgram, errors) = TypeChecker.checkProgram(ast)
    assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
    val result = Interpreter.evalProg(typedProgram, "main")
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
    val (typedProgram, errors) = TypeChecker.checkProgram(ast)
    assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
    val result = Interpreter.evalProg(typedProgram, "main")
    assertEquals(result, Interpreter.Value.IntVal(BigInt(3)))
    assert(errors == List())
  }

  test("type checker can instantiate generic functions") {
    val c = parseInput("""
      |fun id[T](x T) T
      |    x
      |
      |fun main() Int
      |    id(42)
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typedProgram, errors) = TypeChecker.checkProgram(ast)
    assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
    val result = Interpreter.evalProg(typedProgram, "main")
    assertEquals(result, Interpreter.Value.IntVal(BigInt(42)))
    assert(errors == List())
  }


  test("type checker can work with simple data types") {
    val c = parseInput("""
      |data List =
      |   Nil
      | | Cons(head Int, tail List)
      |fun myAppend(a List, b List) List
      |    match a
      |        case Nil
      |          b
      |        case Cons(h, t)
      |          Cons(h, myAppend(t, b))
      |
      |fun main() unit
      |    let lst1 = Cons(1, Cons(2, Nil))
      |    let lst2 = Cons(3, Cons(4, Nil))
      |    let lst3 = myAppend(lst1, lst2)
      |    println(lst3)
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typedProgram, errors) = TypeChecker.checkProgram(ast)
    assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
    println("Evaluating program...")
    val result = Interpreter.evalProg(typedProgram, "main")
    assertEquals(result, Interpreter.Value.UnitVal)
    assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
  }
}

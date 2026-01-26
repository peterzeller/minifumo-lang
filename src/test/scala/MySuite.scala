// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.typing.TypeChecker
import com.github.peterzeller.minifumo.typing.TypedAst
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  // Builds a Nat value for testing.
  private def natValue(value: Int): Interpreter.Value =
    if value <= 0 then
      Interpreter.Value.AdtVal("Zero", Nil)
    else
      Interpreter.Value.AdtVal("Suc", List(natValue(value - 1)))

  // Builds an Int value for testing.
  private def intValue(value: Int): Interpreter.Value =
    val sign = if value >= 0 then Interpreter.Value.AdtVal("True", Nil) else Interpreter.Value.AdtVal("False", Nil)
    Interpreter.Value.AdtVal("Int", List(sign, natValue(math.abs(value))))

  test("interpreter evals main with 1+2") {
    val (c, _) = parseInput("""
      |fun main() Int
      |    1 + 2
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typed, errors) = TypeChecker.checkProgram(ast)
    assertEquals(errors, List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(3))
  }

  test("type checker can type ints") {
    val (c, _) = parseInput("""
      |fun main() Int
      |    let x = 1
      |    let y = 2
      |    x + y
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typed, errors) = TypeChecker.checkProgram(ast)
    assert(errors == List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(3))
  }

  test("type checker can instantiate generic functions") {
    val (c, _) = parseInput("""
      |fun id[T](x T) T
      |    x
      |
      |fun main() Int
      |    id(42)
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typed, errors) = TypeChecker.checkProgram(ast)
    assert(errors == List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(42))
  }


  // test("type checker can work with simple data types") {
  //   val (c, _) = parseInput("""
  //     |data List =
  //     |   Nil
  //     | | Cons(head Int, tail List)
  //     |fun myAppend(a List, b List) List
  //     |    match a
  //     |        case Nil
  //     |          b
  //     |        case Cons(h, t)
  //     |          Cons(h, myAppend(t, b))
  //     |
  //     |instance showList for Show[List] =
  //     |    fun show(self List) String
  //     |        match self
  //     |            case Nil
  //     |                "Nil"
  //     |            case Cons(h, t)
  //     |                "Cons(" + show(h) + ", " + show(t) + ")"
  //     |fun main() unit
  //     |    let lst1 = Cons(1, Cons(2, Nil))
  //     |    let lst2 = Cons(3, Cons(4, Nil))
  //     |    let lst3 = myAppend(lst1, lst2)
  //     |    println(lst3)
  //   """.stripMargin)
  //   val ast = AstTransform.program(c)
  //   val (typed, errors) = TypeChecker.checkProgram(ast)
  //   assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
  //   println("Evaluating program...")
  //   val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
  //   val result = Interpreter.evalProg(combined, "main")
  //   assertEquals(result, Interpreter.Value.UnitVal)
  // }
}

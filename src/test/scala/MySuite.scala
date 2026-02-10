// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.antlr.MinifumoParser
import com.github.peterzeller.minifumo.ast.AstTransform
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.typing.{ProjectSymbolCache, TypeChecker}

import java.nio.file.Path
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

  private val dummyPath: Path = Path.of("dummy.minifumo")
  private val dummyCache = new ProjectSymbolCache(Path.of("."))

  test("interpreter evals main with a simple function call") {
    val (c, _) = parseInput("""
      |fun id(x: Int): Int
      |    x
      |
      |fun main(): Int
      |    id(3)
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, "main")
    assertEquals(result, intValue(3))
  }

  test("type checker can type ints") {
    val (c, _) = parseInput("""
      |fun main(): Int
      |    let x = 1
      |    let y = 2
      |    y
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, "main")
    assertEquals(result, intValue(2))
  }

  test("type checker can handle pattern matching on local data") {
    val (c, _) = parseInput("""
      |data Maybe = None | Some(value: Int)
      |
      |fun main(): Int
      |    match Some(42)
      |        case None
      |            0
      |        case Some(value)
      |            value
    """.stripMargin)
    val ast = AstTransform.program(c)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, "main")
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

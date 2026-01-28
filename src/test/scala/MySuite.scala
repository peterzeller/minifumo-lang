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
  // Parses and type-checks the given source string.
  private def typeCheckSource(source: String): (TypedAst.Program, List[TypeChecker.TypeError]) =
    val (c, _) = parseInput(source)
    val ast = AstTransform.program(c)
    TypeChecker.checkProgram(ast)

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

  test("assignment declares a new binding like let") {
    val (typed, errors) = typeCheckSource("""
      |fun main() Int
      |    x := 1
      |    x
    """.stripMargin)
    assertEquals(errors, List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(1))
  }

  test("uninitialized variables cannot be used") {
    val (_, errors) = typeCheckSource("""
      |fun main() Int
      |    var x Int
      |    x
    """.stripMargin)
    assert(errors.exists(_.message.contains("not initialized")))
  }

  test("initialization on one branch is not enough for use") {
    val (_, errors) = typeCheckSource("""
      |fun main() Int
      |    var x Int
      |    if True then x := 1 else unit
      |    x
    """.stripMargin)
    assert(errors.exists(_.message.contains("initialized")))
  }

  test("immutable vars can only be initialized on one branch") {
    val (_, errors) = typeCheckSource("""
      |fun main() Int
      |    let x Int
      |    if True
      |        x := 1
      |    else
      |        x := 2
      |    x
    """.stripMargin)
    assert(errors.exists(_.message.contains("initialized on multiple branches")))
  }

  test("return exits the function") {
    val (typed, errors) = typeCheckSource("""
      |fun main() Int
      |    let x = 1
      |    return x + 1
      |    99
    """.stripMargin)
    assertEquals(errors, List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(2))
  }

  test("for loops use Iterable instances") {
    val (typed, errors) = typeCheckSource("""
      |fun main() unit
      |    var acc = 0
      |    for value in Cons(1, Cons(2, Nil))
      |        acc := acc + value
      |    println(acc)
    """.stripMargin)
    assertEquals(errors, List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, Interpreter.Value.UnitVal)
  }

  test("field access works for single-constructor types") {
    val (typed, errors) = typeCheckSource("""
      |data Pair = Pair(first Int, second Int)
      |
      |fun main() Int
      |    let p = Pair(1, 2)
      |    p.first + p.second
    """.stripMargin)
    assertEquals(errors, List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(3))
  }

  test("shadowing locals is allowed") {
    val (typed, errors) = typeCheckSource("""
      |fun main() Int
      |    let x = 1
      |    let x = 2
      |    x
    """.stripMargin)
    assertEquals(errors, List())
    val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
    val result = Interpreter.evalProg(combined, "main")
    assertEquals(result, intValue(2))
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

// For more information on writing tests, see
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, ProjectSymbolCache, TypeChecker}

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
    Interpreter.Value.AdtVal("MakeInt", List(sign, natValue(math.abs(value))))

  test("interpreter evals main with a simple function call") {
    val input =
      """
        |fun id(x: Int): Int
        |    x
        |
        |fun main(): Int
        |    id(3)
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(3))
  }

  test("type checker can type ints") {
    val input =
      """
        |fun main(): Int
        |    let x = 1
        |    let y = 2
        |    y
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
        val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(2))
  }

  test("type checker can handle pattern matching on local data") {
    val input =
      """
        |data Maybe = MyNone | MySome(value: Int)
        |
        |fun main(): Int
        |    match MySome(42)
        |        case MyNone
        |            0
        |        case MySome(value)
        |            value
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(42))
  }


  test("constructor result type must match enclosing datatype") {
    val input =
      """
        |data Bad =
        |   MakeBad: Int
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assert(errors.exists(_.message.contains("Constructor MakeBad must return Bad")))
  }

  test("dependent match refines expected branch types for headOrUnit") {
    val input = """
      |fun HeadOrUnit[T: Type](n: Nat): Type
      |    match n
      |        case Zero
      |            Unit
      |        case Suc(_)
      |            T
      |
      |data SizedList[T: Type, N: Nat] =
      |   SizedNil: SizedList[T, Zero]
      | | SizedCons(head: T, tail: SizedList[T, N]): SizedList[T, Suc(N)]
      |
      |fun headOrUnit[T: Type, N: Nat](xs: SizedList[T, N]): HeadOrUnit[T](N)
      |    match xs
      |        case SizedNil
      |            ()
      |        case SizedCons(head, _)
      |            head
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }

  test("dependent recursion with dependent match typechecks") {
    val input =
      """
        |fun RecResult[T: Type](n: Nat): Type
        |    match n
        |        case Zero
        |            Unit
        |        case Suc(_)
        |            T
        |
        |fun recTest[T: Type](n: Nat, value: T): RecResult[T](n)
        |    match n
        |        case Zero
        |            ()
        |        case Suc(k)
        |            value
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    val dummyPath = "test.minifumo"
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }



  test("proposition contexts accept boolean expressions") {
    val input =
      """
        |fun intLeReflexiveChain(x: Int): x <= x ==> x <= x
        |    (h) => h
        |""".stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "bool-prop.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }
  test("standard library compiles without type errors") {
    // Type-checks the bundled standard library in isolation.
    val standardPath = "standard.minifumo"
    val standardCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    val (_, errors) = TypeChecker.checkProgram(
      standardPath,
      Standard.standardProgram,
      standardCache,
      importStandard = false,
      standardCache.ids
    )
    assertEquals(errors, List())
  }

    test("meta logical operators are distinct from boolean operators") {
      val input =
        """
          |fun boolAnd(a: Bool, b: Bool): Bool
          |    a and b
          |
          |fun metaAnd(p: Type, q: Type): And
          |    p AND q
          |
          |fun metaOr(p: Type, q: Type): Or
          |    p OR q
          |""".stripMargin
      val (ast, _) = parseInput(input)
      val dummyPath: String = "dummy.minifumo"
      val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
      dummyCache.addInput(dummyPath, input)
      val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
      assertEquals(errors, List())
  }



  // test("type checker can work with simple data types") {
  //   val (ast, _) = parseInput("""
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
  //     //   val (typed, errors) = TypeChecker.checkProgram(ast)
  //   assert(errors.isEmpty, s"Type errors:\n${errors.mkString("\n")}")
  //   println("Evaluating program...")
  //   val combined = TypedAst.Program(Standard.typedProgram.items ++ typed.items)(typed.source)
  //   val result = Interpreter.evalProg(combined, "main")
  //   assertEquals(result, Interpreter.Value.UnitVal)
  // }
}

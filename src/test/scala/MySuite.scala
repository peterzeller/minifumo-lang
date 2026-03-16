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




  test("match completeness reports missing top-level constructor cases") {
    val input =
      """
        |data Maybe = MyNone | MySome(value: Int)
        |
        |fun main(value: Maybe): Int
        |    match value
        |        case MySome(_)
        |            1
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "missing-top-level-case.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assert(errors.exists(_.message.contains("Non-exhaustive match")))
    assert(errors.exists(_.message.contains("MyNone")))
  }

  test("match completeness reports missing nested constructor cases") {
    val input =
      """
        |data Nat = Zero | Suc(pred: Nat)
        |data PairNat = PairNat(left: Nat, right: Nat)
        |
        |fun main(p: PairNat): Int
        |    match p
        |        case PairNat(Zero, _)
        |            0
        |        case PairNat(Suc(_), Zero)
        |            1
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "missing-nested-case.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assert(errors.exists(_.message.contains("Non-exhaustive match")))
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


  test("parser desugars equals syntax into Eq applications") {
    val input =
      """
        |fun same(x: Int, y: Int): x = y
        |    refl[Int][x]
      """.stripMargin
    val (ast, parseErrors) = parseInput(input)
    assertEquals(parseErrors, List())
    val sig = ast.items.collectFirst { case fun: com.github.peterzeller.minifumo.ast.TopLevel.FunDecl if fun.sig.name == "same" => fun.sig }
    assert(sig.nonEmpty)
    sig.get.returnType match
      case com.github.peterzeller.minifumo.ast.Expr.Call(
            com.github.peterzeller.minifumo.ast.Expr.Call(com.github.peterzeller.minifumo.ast.Expr.Var("Eq"), com.github.peterzeller.minifumo.ast.Expr.Var("x")),
            com.github.peterzeller.minifumo.ast.Expr.Var("y")
          ) => ()
      case other => fail(s"expected Eq call structure, got: $other")
  }

  test("expr printer flattens chained explicit and implicit applications") {
    val src = com.github.peterzeller.minifumo.ast.SourceRange.empty
    val f = com.github.peterzeller.minifumo.ast.Expr.Var("f")(src)
    val x = com.github.peterzeller.minifumo.ast.Expr.Var("x")(src)
    val y = com.github.peterzeller.minifumo.ast.Expr.Var("y")(src)
    val t = com.github.peterzeller.minifumo.ast.Expr.Var("T")(src)
    val u = com.github.peterzeller.minifumo.ast.Expr.Var("U")(src)

    val explicit = com.github.peterzeller.minifumo.ast.Expr.Call(com.github.peterzeller.minifumo.ast.Expr.Call(f, x)(src), y)(src)
    assertEquals(explicit.toString, "f(x, y)")

    val mixed = com.github.peterzeller.minifumo.ast.Expr.Call(
      com.github.peterzeller.minifumo.ast.Expr.CallImplicit(
        com.github.peterzeller.minifumo.ast.Expr.CallImplicit(f, t)(src),
        u
      )(src),
      x
    )(src)
    assertEquals(mixed.toString, "f[T, U](x)")

    val eqExpr = com.github.peterzeller.minifumo.ast.Expr.Call(
      com.github.peterzeller.minifumo.ast.Expr.Call(com.github.peterzeller.minifumo.ast.Expr.Var("Eq")(src), x)(src),
      y
    )(src)
    assertEquals(eqExpr.toString, "x = y")
  }

  test("Eq refl pattern with omitted implicit datatype params matches at runtime") {
    val input =
      """
        |fun pickWithEq(x: Int, proof: Eq(x, x)): Int
        |    match proof
        |        case refl
        |            x
        |
        |fun main(): Int
        |    pickWithEq(4, refl[Int][4])
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(4))
  }



  test("datatype constructors can omit implicit datatype args in return types") {
    val input =
      """
        |data Eq2[T: Type](a: T, b: T) =
        |   refl2: Eq2(a, a)
        |
        |fun pickWithEq2(x: Int, proof: Eq2(x, x)): Int
        |    match proof
        |        case refl2
        |            x
        |
        |fun main(): Int
        |    pickWithEq2(4, refl2[Int][4])
    """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath: String = "dummy-eq2.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(4))
  }


  test("subst infers motive through higher-order matching") {
    val input =
      """
        |fun bool_distinct[a: Bool](h1: Eq(a, True), h2: Eq(a, False)): FalseProp
        |    match a
        |        case True
        |            let h: Eq(a, True) = match_Eq
        |            let q: Eq(True, False) = subst(match_Eq, h2)
        |            match q
        |        case False
        |            let h: Eq(a, False) = match_Eq
        |            let q: Eq(False, True) = subst(match_Eq, h1)
        |            match q
      """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "subst-motive-inference.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }



  test("single-constructor datatype fields are accessible via dot syntax") {
    val input =
      """
        |data Pair = MakePair(left: Int, right: Int)
        |
        |fun pickLeft(p: Pair): Int
        |    p.left
        |
        |fun main(): Int
        |    pickLeft(MakePair(5, 7))
      """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "pair-dot.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(5))
  }

  test("single-constructor datatype fields have generated accessor functions") {
    val input =
      """
        |data Pair = MakePair(left: Int, right: Int)
        |
        |fun main(): Int
        |    Pair_left(MakePair(9, 3))
      """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "pair-accessor.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typed, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
    val result = Interpreter.evalProg(typed, dummyCache, "main")
    assertEquals(result, intValue(9))
  }
  
  test("axiom expression type checks against any expected term") {
    val input =
      """
        |fun chooseInt(): Int
        |    axiom
        |
        |fun chooseEq(): Eq[Int](1, 1)
        |    axiom
      """.stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "axiom-test.minifumo"
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
          |fun metaAnd(p: Type, q: Type): Type
          |    p AND q
          |
          |fun metaOr(p: Type, q: Type): Type
          |    p OR q
          |""".stripMargin
      val (ast, _) = parseInput(input)
      val dummyPath: String = "dummy.minifumo"
      val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
      dummyCache.addInput(dummyPath, input)
      val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
      assertEquals(errors, List())
  }



  test("lemma syntax supports optional given and assumes sections") {
    val input =
      """
        |lemma idLemma:
        |    shows Type
        |    proof
        |        Type
      """.stripMargin
    val (ast, parseErrors) = parseInput(input)
    assertEquals(parseErrors, List())
    val dummyPath = "lemma-optional-sections.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }

  test("lemma syntax desugars to a function declaration") {
    val input =
      """
        |lemma andComm:
        |    given A: Type, B: Type
        |    assumes h: A AND B
        |    shows B AND A
        |    proof
        |        match h
        |            case MakeAnd(a, b)
        |                MakeAnd(b, a)
      """.stripMargin
    val (ast, parseErrors) = parseInput(input)
    assertEquals(parseErrors, List())
    val dummyPath = "lemma-sugar.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }

  test("meta iff operator parses and type checks") {
    val input =
      """
        |fun iffType(p: Type, q: Type): Type
        |    p <==> q
        |
        |fun iffValue(p: Type, q: Type, pq: p ==> q, qp: q ==> p): Iff[p, q]
        |    MakeIff[p, q](pq, qp)
        |""".stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "meta-iff.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (_, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())
  }

  test("meta iff has lower precedence than implies") {
    val input =
      """
        |fun iffPrecedence(p: Type, q: Type, r: Type): Type
        |    p ==> q <==> r
        |""".stripMargin
    val (ast, _) = parseInput(input)
    val dummyPath = "meta-iff-precedence.minifumo"
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

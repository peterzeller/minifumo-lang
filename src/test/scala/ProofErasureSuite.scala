import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, ProjectSymbolCache, ProofErasure, TypeChecker, TypedAst}

class ProofErasureSuite extends munit.FunSuite:

  test("proof erasure removes proposition constructors and function type params") {
    val input =
      """
        |data Eq[T: Type](a: T, b: T): Prop =
        |  refl: Eq[T](a, a)
        |
        |fun useEq[T: Type](x: T, p: Eq[T](x, x)): T
        |  match p
        |    case refl
        |      x
        |""".stripMargin
    val (ast, parseErrors) = parseInput(input)
    assertEquals(parseErrors, List())
    val dummyPath = "proof-erasure-suite.minifumo"
    val dummyCache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    dummyCache.addInput(dummyPath, input)
    val (typedProgram, errors) = TypeChecker.checkProgram(dummyPath, ast, dummyCache, true, dummyCache.ids)
    assertEquals(errors, List())

    val erased = ProofErasure.erase(typedProgram)
    val dataDecl = erased.items.collectFirst { case d: TypedAst.TopLevel.DataDecl => d }.get
    assertEquals(dataDecl.typeParams, List())
    assertEquals(dataDecl.ctors.map(_.fields.length), List(0))

    val funDecl = erased.items.collectFirst { case f: TypedAst.TopLevel.FunDecl => f }.get
    assertEquals(funDecl.sig.typeParams, List())
    assertEquals(funDecl.sig.params.length, 1)
  }

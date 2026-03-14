import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{DatatypeIndexErasure, GlobalSymbolsIo, ProjectSymbolCache, TypeChecker, TypedAst}

class DatatypeIndexErasureSuite extends munit.FunSuite:

  test("datatype index erasure keeps only type parameters") {
    val input =
      """
        |data SizedVec[T: Type, N: Nat] =
        |  Empty: SizedVec[T, Zero]
        |""".stripMargin

    val (ast, parseErrors) = parseInput(input)
    assertEquals(parseErrors, List())

    val dummyPath = "datatype-index-erasure-suite.minifumo"
    val cache = new ProjectSymbolCache(GlobalSymbolsIo.create("."), TypeChecker.IdSupply())
    cache.addInput(dummyPath, input)

    val (typedProgram, typeErrors) = TypeChecker.checkProgram(dummyPath, ast, cache, true, cache.ids)
    assertEquals(typeErrors, List())

    val erasedProgram = DatatypeIndexErasure.erase(typedProgram)
    val sizedVecDecl = erasedProgram.items.collectFirst {
      case dataDecl: TypedAst.TopLevel.DataDecl if dataDecl.symbol.name == "SizedVec" => dataDecl
    }.get

    assertEquals(sizedVecDecl.typeParams.map(_.name), List("T"))
  }

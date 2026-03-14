import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{DatatypeIndexErasure, DatatypeSymbol, GlobalSymbolsIo, ProjectSymbolCache, TypeChecker, TypedAst}

class DatatypeIndexErasureSuite extends munit.FunSuite:

  test("datatype index erasure rewrites declarations and datatype applications") {
    val input =
      """
        |data SizedVec[T: Type, N: Nat] =
        |   SEmpty: SizedVec[T, Zero]
        | | SCons(head: T, tail: SizedVec[T, N]): SizedVec[T, Suc(N)]
        |
        |fun append[T: Type, N: Nat, M: Nat](xs: SizedVec[T, N], ys: SizedVec[T, M]): SizedVec[T, natAdd(N, M)]
        |  match xs
        |    case SEmpty
        |      ys
        |    case SCons(head, tail)
        |      SCons[T][natAdd(N, M)](head, append[T][N][M](tail, ys))
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

    val appendDecl = erasedProgram.items.collectFirst {
      case funDecl: TypedAst.TopLevel.FunDecl if funDecl.sig.symbol.name == "append" => funDecl
    }.get

    val sizedVecArgCounts = collectDatatypeApplicationArgCounts(appendDecl, "SizedVec")
    assert(sizedVecArgCounts.nonEmpty)
    assertEquals(sizedVecArgCounts.distinct, List(1))
  }

  /** Collects argument counts for every application whose head is the named datatype symbol. */
  private def collectDatatypeApplicationArgCounts(node: TypedAst, datatypeName: String): List[Int] =
    node match
      case expr: TypedAst.Expr =>
        val (head, args) = flattenApplication(expr)
        val current = head match
          case TypedAst.Expr.Var(symbol: DatatypeSymbol) if symbol.name == datatypeName && args.nonEmpty => List(args.length)
          case _ => List()
        current ++ expr.children.flatMap(child => collectDatatypeApplicationArgCounts(child, datatypeName))
      case _ =>
        node.children.flatMap(child => collectDatatypeApplicationArgCounts(child, datatypeName))

  /** Flattens nested explicit and implicit applications into one head expression and argument list. */
  private def flattenApplication(expr: TypedAst.Expr): (TypedAst.Expr, List[TypedAst.Expr]) =
    def loop(current: TypedAst.Expr, reversedArgs: List[TypedAst.Expr]): (TypedAst.Expr, List[TypedAst.Expr]) =
      current match
        case TypedAst.Expr.App(callee, arg, _) => loop(callee, arg :: reversedArgs)
        case TypedAst.Expr.AppImplicit(callee, arg, _) => loop(callee, arg :: reversedArgs)
        case _ => (current, reversedArgs)
    loop(expr, List())

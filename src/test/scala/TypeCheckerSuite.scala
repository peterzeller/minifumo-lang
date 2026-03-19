import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.typing.TypeChecker.IdSupply
import com.github.peterzeller.minifumo.typing.TypedAst.TopLevel.FunDecl
import com.github.peterzeller.minifumo.typing.{GlobalSymbolsIo, LocalSymbol, ProjectSymbolCache, Symbol, TermSymbol, TypeChecker, TypedAst}

import scala.collection.mutable

class TypeCheckerSuite extends munit.FunSuite:

  // Builds an empty typing context for normalization tests.
  private def emptyContext: TypeChecker.Context =
    new TypeChecker.Context:
      override def lookupSymbol(name: String): Option[Symbol] = None
      override def lookupValue(symbol: TermSymbol): Option[TypedAst.Expr] = None
      override def lookupDefinition(symbol: Symbol): Option[TypedAst.Expr] = None

  // Builds a mutable meta context for normalization tests.
  private def metaContext: TypeChecker.MetaContext =
    val assignments = mutable.Map[Int, TypedAst.Expr]()
    val constraintsBuf = mutable.ListBuffer[TypeChecker.Constraint]()
    new TypeChecker.MetaContext:
      override def assign(metaId: Int, term: TypedAst.Expr): Unit = assignments.update(metaId, term)
      override def getAssignment(metaId: Int): Option[TypedAst.Expr] = assignments.get(metaId)
      override def addConstraint(constraint: TypeChecker.Constraint): Unit = constraintsBuf.addOne(constraint)
      override def constraints: List[TypeChecker.Constraint] = constraintsBuf.toList

  test("whnf reduces lambda application") {
    val source = ast.SourceRange.empty
    val param = LocalSymbol("x", TypedAst.Expr.UnknownType()(source), 0)("")
    val body = TypedAst.Expr.Var(param)(source)
    val lambda = TypedAst.Expr.Lambda(param, body, TypedAst.Expr.UnknownType()(source))(source)
    val literal = TypedAst.Expr.Lit(ast.Literal.IntLit("1")(source))(source)
    val term = TypedAst.Expr.App(lambda, literal, TypedAst.Expr.UnknownType()(source))(source)
    given TypeChecker.Context = emptyContext
    given TypeChecker.MetaContext = metaContext
    val reduced = TypeChecker.whnf(term)
    assertEquals(reduced, literal)
  }

  test("substitution replaces matching symbols") {
    val source = ast.SourceRange.empty
    val symbol = LocalSymbol("x", TypedAst.Expr.UnknownType()(source), 1)("")
    val other = LocalSymbol("y", TypedAst.Expr.UnknownType()(source), 2)("")
    val value = TypedAst.Expr.Lit(ast.Literal.IntLit("42")(source))(source)
    val term = TypedAst.Expr.App(
      TypedAst.Expr.Var(symbol)(source),
      TypedAst.Expr.Var(other)(source),
      TypedAst.Expr.UnknownType()(source)
    )(source)
    val substituted = TypeChecker.substituteForTest(term, symbol, value)
    val expected = TypedAst.Expr.App(
      value,
      TypedAst.Expr.Var(other)(source),
      TypedAst.Expr.UnknownType()(source)
    )(source)
    assertEquals(substituted, expected)
  }

//  test("infer synthesizes literal types") {
//    val program = parseProgram("""
//      |fun main(): Int
//      |  1
//    """.stripMargin)
//    val expr = firstFunBody(program)
//    val exports = TypeChecker.withStandardExports(TypeChecker.emptyExportEnv)
//    val result = TypeChecker.inferInTestContext(program, expr, exports)
//    val inferredType = result.toOption.map(_._2)
//    val inferredName = inferredType.collect { case TypedAst.Expr.Var(symbol) => symbol.name }
//    assertEquals(inferredName, Some("Int"))
//  }

//  test("check validates literals against expected types") {
//    val program = parseProgram("""
//      |fun main(): Int
//      |  1
//    """.stripMargin)
//    val expr = firstFunBody(program)
//    val exports = TypeChecker.withStandardExports(TypeChecker.emptyExportEnv)
//    val result = TypeChecker.checkInTestContext(program, expr, "Int", exports)
//    assert(result.isRight)
//  }

//  test("check handles lambdas when expected type is a function") {
//    val program = parseProgram("""
//      |fun main(): Int -> Int
//      |  x: Int => x
//    """.stripMargin)
//    val (typed, errors) = TypeChecker.checkProgram(program, TypeChecker.emptyExportEnv)
//    assertEquals(errors, List())
//    val funBody = typed.items.collectFirst { case TypedAst.TopLevel.FunDecl(_, _, _, body) => body }
//    assert(funBody.exists(_.isInstanceOf[TypedAst.Expr.Lambda]))
//  }


  test("isDefEq solves metas during definitional equality checks") {
    val source = ast.SourceRange.empty
    val meta = TypedAst.Expr.Meta(0, TypedAst.Expr.UnknownType()(source))("T", source)
    val literal = TypedAst.Expr.Lit(ast.Literal.IntLit("7")(source))(source)
    given TypeChecker.Context = emptyContext
    val assignments = mutable.Map[Int, TypedAst.Expr]()
    val constraintsBuf = mutable.ListBuffer[TypeChecker.Constraint]()
    given TypeChecker.MetaContext = new TypeChecker.MetaContext:
      override def assign(metaId: Int, term: TypedAst.Expr): Unit = assignments.update(metaId, term)
      override def getAssignment(metaId: Int): Option[TypedAst.Expr] = assignments.get(metaId)
      override def addConstraint(constraint: TypeChecker.Constraint): Unit = constraintsBuf.addOne(constraint)
      override def constraints: List[TypeChecker.Constraint] = constraintsBuf.toList
    val result = TypeChecker.isDefEq(meta, literal, source)
    assert(result)
    assertEquals(assignments.get(0), Some(literal))
    assertEquals(constraintsBuf.length, 0)
  }

  test("pretty print of eq constraints") {
    val ids = IdSupply()
    val cache = ProjectSymbolCache(GlobalSymbolsIo.create("."), ids)
    val (typed, errs) = TypeChecker.checkProgram("standard.minifumo", Standard.standardProgram, cache, false, ids)
    require(errs.isEmpty)
    val funcs = typed.items.flatMap {
      case f: FunDecl => List(f)
      case _ => List()
    }
    val substFunc = funcs.find(_.sig.symbol.name == "subst").get
    val paramsStr = substFunc.sig.params.map(p => s"$p: ${p.tpe}").mkString(", ")
    require(paramsStr == "h: (x = y), hx: motive(x)")

  }

//  test("pattern matching substitutes constructor type parameters without standard library") {
//    val program = parseProgram("""
//      |data Type = Type
//      |data Num = Num
//      |data Foo[T: Type] = Bar(t: T)
//      |
//      |fun bla(f: Foo[Num]): Num
//      |  match f
//      |    case Bar(x)
//      |      x
//    """.stripMargin)
//    val (_, errors) = TypeChecker.checkProgramWithoutStandard(program, TypeChecker.emptyExportEnv)
//    assertEquals(errors, List())
//  }

//  test("can construct generic list") {
//    val program = parseProgram("""
//      |data Type = makeType
//      |data Num = makeNum
//      |data Foo[T: Type] = makeFoo(t: T)
//      |
//      |fun test(): Foo[Foo[Num]]
//      |  let x: Foo[Foo[Num]] = makeFoo[Foo[Num]](makeFoo[Num](makeNum))
//      |  x
//    """.stripMargin)
//    val (_, errors) = TypeChecker.checkProgramWithoutStandard(program, TypeChecker.emptyExportEnv)
//    assertEquals(errors, List())
//  }

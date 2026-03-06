import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.{GlobalName, GlobalSymbol, NameCache, SymbolCache, TypeChecker}


class WebRuntimeStandardLibrarySuite extends munit.FunSuite:

  // Builds a Nat runtime value for integer assertions.
  private def natValue(value: Int): Interpreter.Value =
    if value <= 0 then
      Interpreter.Value.AdtVal("Zero", Nil)
    else
      Interpreter.Value.AdtVal("Suc", List(natValue(value - 1)))

  // Builds an Int runtime value for interpreter assertions.
  private def intValue(value: Int): Interpreter.Value =
    val sign = if value >= 0 then Interpreter.Value.AdtVal("True", Nil) else Interpreter.Value.AdtVal("False", Nil)
    Interpreter.Value.AdtVal("MakeInt", List(sign, natValue(math.abs(value))))

  // Creates an empty symbol cache that mirrors the browser compiler setup.
  private object EmptyCache extends NameCache with SymbolCache:
    override def globalNames(path: String): Map[String, GlobalName] =
      Map.empty

    override def globalSymbols(path: String): Map[String, GlobalSymbol] =
      Map.empty

    override def globalEnv(path: String): TypeChecker.GlobalEnv =
      TypeChecker.GlobalEnv(Map())

//  test("standalone interpreter run can use standard library operators when bundled runtime is loaded") {
//    val (program, parseErrors) = parseInput(
//      """fun main(): Int
//        |  1 + 2
//        |""".stripMargin
//    )
//    assertEquals(parseErrors, List.empty)
//
//    val ids = TypeChecker.IdSupply()
//    val (typedProgram, typeErrors) = TypeChecker.checkProgram("/playground/input.minifumo", program, EmptyCache, importStandard = true, ids)
//    assertEquals(typeErrors, List.empty)
//
//    val (typedStandardProgram, standardErrors) = TypeChecker.checkProgram("standard.minifumo", Standard.standardProgram, EmptyCache, importStandard = false, ids)
//    assertEquals(standardErrors, List.empty)
//
//    val result = Interpreter.evalProg(typedProgram, List(typedStandardProgram), "main")
//    assertEquals(result, intValue(3))
//  }

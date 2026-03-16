import com.github.peterzeller.minifumo.ast.{Expr, TopLevel}
import com.github.peterzeller.minifumo.parser.parseInput

class ParserDocCommentSuite extends munit.FunSuite:
  test("parser stores doc comments for top-level, constructors, and let binders") {
    val input =
      """// datatype docs
        |data Box =
        |  // ctor docs
        |  MkBox(value: Int)
        |
        |// function docs
        |fun main(): Int
        |  // let docs
        |  let x: Int = 1 in x
        |""".stripMargin

    val (program, errors) = parseInput(input)
    assertEquals(errors, Nil)

    val data = program.items.head.asInstanceOf[TopLevel.DataDecl]
    assert(data.ctors.nonEmpty)

    val fun = program.items(1).asInstanceOf[TopLevel.FunDecl]
    fun.body.asInstanceOf[Expr.LetIn]
  }

package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.ast.Literal
import com.github.peterzeller.minifumo.typing.TypedAst
import com.github.peterzeller.minifumo.typing.TypedAst.*

import scala.util.control.NoStackTrace

object Interpreter:
  enum Value:
    case AdtVal(name: String, args: List[Value])
    case UnitVal
    case FuncVal(
        name: String,
        fn: Value => Value
      )
    case UndefinedVal

    override def toString: String =
      this match
        case Value.AdtVal(name, args) =>
          if args.isEmpty then name else s"$name${args.map(_.toString).mkString("(", ", ", ")")}"
        case Value.UnitVal => "unit"
        case Value.UndefinedVal => "undefined"
        case Value.FuncVal(name, f) => s"<function $name>"


  // evaluate a function from the program
  def evalProg(prog: TypedAst.Program, funcName: String): Value =
    ???
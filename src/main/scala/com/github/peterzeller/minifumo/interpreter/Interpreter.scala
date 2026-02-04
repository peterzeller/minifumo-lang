package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.ast.Literal
import com.github.peterzeller.minifumo.typing.TypedAst
import com.github.peterzeller.minifumo.typing.TypedAst.*

import scala.collection.mutable
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

  /** Evaluates a function from the program by name. */
  def evalProg(prog: TypedAst.Program, funcName: String): Value =
    val (functionBodies, ctorArities) = indexProgram(prog)
    val globals = mutable.Map[TypedAst.Symbol, Value]()
    ctorArities.foreach { case (symbol, arity) =>
      globals.update(symbol, buildCtorValue(symbol.name, arity))
    }
    functionBodies.foreach { case (symbol, info) =>
      globals.update(symbol, buildFunctionValue(symbol.name, info.params, info.body, globals))
    }
    val entry = functionBodies.keys.find(_.name == funcName)
    entry match
      case Some(symbol) =>
        globals.getOrElse(symbol, Value.UndefinedVal)
      case None =>
        throw new RuntimeException(s"Unknown function $funcName") with NoStackTrace

  /** Describes a function body for evaluation. */
  private final case class FunctionBody(params: List[TypedAst.ParamSymbol], body: TypedAst.Expr)

  /** Indexes functions and constructor arities in a typed program. */
  private def indexProgram(prog: TypedAst.Program): (Map[TypedAst.FunctionSymbol, FunctionBody], Map[TypedAst.CtorSymbol, Int]) =
    val functions = mutable.Map[TypedAst.FunctionSymbol, FunctionBody]()
    val ctors = mutable.Map[TypedAst.CtorSymbol, Int]()
    prog.items.foreach {
      case TypedAst.TopLevel.FunDecl(symbol, _, params, body) =>
        functions.update(symbol, FunctionBody(params, body))
      case TypedAst.TopLevel.DataDecl(_, _, ctorDecls) =>
        ctorDecls.foreach { ctor =>
          ctors.update(ctor.symbol, ctor.fields.size)
        }
    }
    (functions.toMap, ctors.toMap)

  /** Builds a curried constructor value with the given arity. */
  private def buildCtorValue(name: String, arity: Int): Value =
    def loop(args: List[Value]): Value =
      if args.length >= arity then
        Value.AdtVal(name, args.reverse)
      else
        Value.FuncVal(name, value => loop(value :: args))
    loop(Nil)

  /** Builds a curried function value from its parameters and body. */
  private def buildFunctionValue(
      name: String,
      params: List[TypedAst.ParamSymbol],
      body: TypedAst.Expr,
      globals: mutable.Map[TypedAst.Symbol, Value]
    ): Value =
    def loop(remaining: List[TypedAst.ParamSymbol], localEnv: Map[TypedAst.TermSymbol, Value]): Value =
      remaining match
        case Nil => evalExpr(body, localEnv, globals)
        case param :: tail =>
          Value.FuncVal(param.name, value => loop(tail, localEnv + (param -> value)))
    loop(params, Map())

  /** Evaluates an expression with the given local and global environments. */
  private def evalExpr(
      expr: TypedAst.Expr,
      locals: Map[TypedAst.TermSymbol, Value],
      globals: mutable.Map[TypedAst.Symbol, Value]
    ): Value =
    expr match
      case TypedAst.Expr.Lit(value) => literalValue(value)
      case TypedAst.Expr.Var(symbol: TypedAst.TermSymbol) =>
        locals.getOrElse(symbol, globals.getOrElse(symbol, Value.UndefinedVal))
      case TypedAst.Expr.Var(symbol) =>
        globals.getOrElse(symbol, Value.UndefinedVal)
      case TypedAst.Expr.Var(_) => Value.UndefinedVal
      case TypedAst.Expr.App(callee, arg, _) =>
        val fn = evalExpr(callee, locals, globals)
        val argVal = evalExpr(arg, locals, globals)
        applyValue(fn, argVal)
      case TypedAst.Expr.AppImplicit(callee, _, _) =>
        evalExpr(callee, locals, globals)
      case TypedAst.Expr.Lambda(param, body, _) =>
        Value.FuncVal(param.name, value => evalExpr(body, locals + (param -> value), globals))
      case TypedAst.Expr.LetIn(symbol, _, _, value, body) =>
        val valueVal = evalExpr(value, locals, globals)
        evalExpr(body, locals + (symbol -> valueVal), globals)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val scrutineeVal = evalExpr(scrutinee, locals, globals)
        evalMatch(scrutineeVal, cases, locals, globals)
      case TypedAst.Expr.Meta(_, _) => Value.UndefinedVal
      case TypedAst.Expr.Sort() => Value.UndefinedVal
      case TypedAst.Expr.Pi(_, _, _) => Value.UndefinedVal
      case TypedAst.Expr.UnknownType() => Value.UndefinedVal

  /** Applies a function value to an argument value. */
  private def applyValue(fn: Value, arg: Value): Value =
    fn match
      case Value.FuncVal(_, f) => f(arg)
      case Value.UndefinedVal => Value.UndefinedVal
      case _ => Value.UndefinedVal

  /** Evaluates pattern matching cases against a scrutinee value. */
  private def evalMatch(
      scrutinee: Value,
      cases: List[TypedAst.MatchCase],
      locals: Map[TypedAst.TermSymbol, Value],
      globals: mutable.Map[TypedAst.Symbol, Value]
    ): Value =
    cases match
      case Nil => Value.UndefinedVal
      case head :: tail =>
        matchPattern(head.pattern, scrutinee) match
          case Some(bindings) =>
            val merged = locals ++ bindings
            evalExpr(head.body, merged, globals)
          case None =>
            evalMatch(scrutinee, tail, locals, globals)

  /** Matches a pattern against a value, returning bindings on success. */
  private def matchPattern(pattern: TypedAst.Pattern, value: Value): Option[Map[TypedAst.TermSymbol, Value]] =
    pattern match
      case TypedAst.Pattern.Wildcard() => Some(Map())
      case TypedAst.Pattern.Lit(lit) =>
        val litVal = literalValue(lit)
        if value == litVal then Some(Map()) else None
      case TypedAst.Pattern.Binder(symbol) => Some(Map(symbol -> value))
      case TypedAst.Pattern.Ctor(symbol, args) =>
        value match
          case Value.AdtVal(name, values) if name == symbol.name && values.length == args.length =>
            val maybeBindings = args.zip(values).foldLeft(Option(Map[TypedAst.TermSymbol, Value]())) {
              case (Some(bindings), (pat, v)) =>
                matchPattern(pat, v).map(bindings ++ _)
              case (None, _) => None
            }
            maybeBindings
          case _ => None

  /** Converts a literal into its runtime value representation. */
  private def literalValue(lit: Literal): Value =
    lit match
      case Literal.IntLit(value) =>
        val number = value.toInt
        val positive = if number >= 0 then Value.AdtVal("True", Nil) else Value.AdtVal("False", Nil)
        Value.AdtVal("Int", List(positive, natValue(math.abs(number))))
      case Literal.BoolLit(value) =>
        if value then Value.AdtVal("True", Nil) else Value.AdtVal("False", Nil)
      case Literal.StringLit(value) =>
        val chars = value.toList.map(ch => Value.AdtVal("Char", List(natValue(ch.toInt))))
        Value.AdtVal("String", List(listValue(chars)))
      case Literal.UnitLit() => Value.UnitVal

  /** Builds a Nat ADT value from an integer. */
  private def natValue(value: Int): Value =
    if value <= 0 then
      Value.AdtVal("Zero", Nil)
    else
      Value.AdtVal("Suc", List(natValue(value - 1)))

  /** Builds a List ADT value from elements. */
  private def listValue(values: List[Value]): Value =
    values match
      case Nil => Value.AdtVal("Nil", Nil)
      case head :: tail => Value.AdtVal("Cons", List(head, listValue(tail)))

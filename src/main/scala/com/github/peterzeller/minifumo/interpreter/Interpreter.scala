package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.ast.Literal
import com.github.peterzeller.minifumo.typing.{ProjectSymbolCache, TypedAst}
import com.github.peterzeller.minifumo.typing.TypedAst.*
import com.github.peterzeller.minifumo.typing.TypedAst.TopLevel.FunDecl

import scala.collection.mutable

object Interpreter:
  enum Value:
    case AdtVal(name: String, args: List[Value])
    case UnitVal
    case FuncVal(
        name: String,
        fn: Value => Value
      )
    case LazyVal(name: String, fn: () => Value)
    case UndefinedVal

    override def toString: String =
      this match
        case Value.AdtVal(name, args) =>
          if args.isEmpty then name else s"$name${args.map(_.toString).mkString("(", ", ", ")")}" 
        case Value.UnitVal => "unit"
        case Value.UndefinedVal => "undefined"
        case Value.FuncVal(name, _) => s"<function $name>"

  /** Evaluates a function from the program by name. */
  def evalProg(prog: TypedAst.Program, symbols: ProjectSymbolCache, funcName: String): Value =
    val f = prog.items.collectFirst { case f@FunDecl(s, _) if s.symbol.name == funcName => f }.get
    val locals = Map[TermSymbol, Value]()
    val globals = buildGlobalTable(prog, symbols)
    evalExpr(f.body, locals, globals)

  private def buildGlobalTable(program: Program, symbols: ProjectSymbolCache): mutable.Map[Symbol, Value] =
    val res = mutable.Map[Symbol, Value]()

    println(s"all paths = ${symbols.allPaths}")
    for path <- symbols.allPaths do
      val (t, _) = symbols.typedAst(path)
      println(s"Adding names from $path")
      for p <- t.items do
        p match
          case TypedAst.TopLevel.DataDecl(name, typeParams, ctors) =>
            // TODO add names for data decl
          case TypedAst.TopLevel.FunDecl(sig, body) =>
            val params = sig.typeParams ++ sig.params
            val fnBody: Value =
              if params.isEmpty then
                Value.LazyVal(sig.symbol.name, () => evalExpr(body, Map(), res))
              else
                buildFnBody(sig.symbol.name, params, body, Map(), res)

            res.put(sig.symbol, fnBody)
    res

  def buildFnBody(name: String, params: List[LocalSymbol], body: Expr, locals: Map[TermSymbol, Value], globals: mutable.Map[Symbol, Value]): Value =
    params match
      case Nil =>
        evalExpr(body, locals, globals)
      case p :: ps =>
        Value.FuncVal(name, v => buildFnBody(name + "'", ps, body, locals + (p -> v), globals))

  /** Describes a function body for evaluation. */
  private final case class FunctionBody(params: List[TypedAst.LocalSymbol], body: TypedAst.Expr)

  /** Evaluates an expression with the given local and global environments. */
  private def evalExpr(
      expr: TypedAst.Expr,
      locals: Map[TypedAst.TermSymbol, Value],
      globals: mutable.Map[TypedAst.Symbol, Value]
    ): Value =
    val res = expr match
      case TypedAst.Expr.Lit(value) => literalValue(value)
      case TypedAst.Expr.Var(symbol: TypedAst.TermSymbol) =>
        locals.getOrElse(symbol, globals.getOrElse(symbol, throw RuntimeException(s"Could not find var $symbol")))
      case TypedAst.Expr.Var(symbol) =>
        globals.getOrElse(symbol, {
          // TODO should not be necessary to search by name, we need to unify symbols
          val byName = globals.find(s => s._1.name == symbol.name)
          byName match {
            case Some(value) => value._2
            case None =>
              throw RuntimeException(s"Could not find global var ${symbol.name} in [${globals.keySet.map(_.name).toList.sorted.mkString(", ")}]")
          }
        })
      case TypedAst.Expr.App(callee, arg, _) =>
        val fn = evalExpr(callee, locals, globals)
        val argVal = evalExpr(arg, locals, globals)
        applyValue(fn, argVal)
      case TypedAst.Expr.AppImplicit(f, arg, _) =>
        val fn = evalExpr(f, locals, globals)
        val argVal = evalExpr(arg, locals, globals)
        applyValue(fn, argVal)
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
    println(s"Evaluating ${prettyPrint(expr)}\n-> $res")
    res

  private def prettyPrint(expr: Expr): String =
    expr match {
      case TypedAst.Expr.Lit(value) => value.toString
      case TypedAst.Expr.Var(symbol) => symbol.name
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) => prettyPrint(callee) + "[" + prettyPrint(arg) + "]"
      case TypedAst.Expr.App(callee, arg, tpe) => prettyPrint(callee) + "(" + prettyPrint(arg) + ")"
      case TypedAst.Expr.Pi(dom, cod, isImplicit) => "PI " + dom.name + ". " +  prettyPrint(dom.tpe) + " -> " + prettyPrint(cod)
      case TypedAst.Expr.Sort() => "Sort"
      case TypedAst.Expr.Lambda(param, body, tpe) => "fun " + param.name + ". " +  prettyPrint(param.tpe) + " -> " + prettyPrint(body)
      case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) => "let " + symbol.name + ": " +  prettyPrint(symbol.tpe) + " = " + prettyPrint(value) + " in " + prettyPrint(body)
      case TypedAst.Expr.Meta(index, tpe) => s"META_$index"
      case TypedAst.Expr.UnknownType() => "???"
      case TypedAst.Expr.Match(scrutinee, cases) => "match ..."
    }

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

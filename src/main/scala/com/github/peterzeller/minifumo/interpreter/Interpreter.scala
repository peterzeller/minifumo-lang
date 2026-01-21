package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.ast.*

import scala.annotation.tailrec
import scala.util.control.NoStackTrace

object Interpreter:
  enum Value:
    case IntVal(value: BigInt)
    case BoolVal(value: Boolean)
    case StringVal(value: String)
    case ListVal(value: Vector[Value])
    case SetVal(value: Set[Value])
    case MapVal(value: Map[Value, Value])
    case AdtVal(name: String, args: List[Value])
    case UnitVal
    case FuncVal(fn: (List[Value], Env) => (Value, Env))
    case UndefinedVal

    override def toString(): String =
      renderValue(this)

  final case class FunDef(params: List[String], body: Suite)

  final case class Env(
      scopes: List[Map[String, Value]],
      functions: Map[String, FunDef],
      ctors: Map[String, Int]
    ):
    def resolve(name: String): Option[Value] =
      scopes.collectFirst { case scope if scope.contains(name) => scope(name) }.orElse({
        functions.get(name).map(f => {
          Value.FuncVal((args: List[Value], env: Env) => {
            if args.length != f.params.length then
              throw new IllegalArgumentException(s"Function $name expects ${f.params.length} args, got ${args.length}")
            val bindings = f.params.zip(args).toMap
            val envWithParams = env.pushScope(bindings)
            try
              val (result, envAfter) = Interpreter.evalSuite(f.body, envWithParams)
              (result, envAfter.popScope)
            catch
              case Interpreter.ReturnSignal(value, envAfter) =>
                (value, envAfter.popScope)
          })
        })
      }).orElse({
        ctors.get(name).map(arity =>
          if arity == 0 then
            Value.AdtVal(name, Nil)
          else
            Value.FuncVal((args: List[Value], env: Env) => {
            if args.length != arity then
              throw new IllegalArgumentException(s"Constructor $name expects $arity args, got ${args.length}")
            (Value.AdtVal(name, args), env)
        }))
      })

    def pushScope(bindings: Map[String, Value]): Env =
      copy(scopes = bindings :: scopes)

    def popScope: Env =
      copy(scopes = scopes.drop(1))

    def withBinding(name: String, value: Value): Env =
      scopes match
        case head :: tail => copy(scopes = (head + (name -> value)) :: tail)
        case Nil => copy(scopes = List(Map(name -> value)))

  private final case class ReturnSignal(value: Value, env: Env) extends RuntimeException with NoStackTrace

  type Builtin = (List[Value], Env) => (Value, Env)

  def evalProg(program: ProgramFile, entryName: String): Value =
    val env = buildEnv(program)
    val (value, _) = evalFunc(entryName, Nil, env)
    value

  def evalFunc(name: String, args: List[Value], env: Env): (Value, Env) =
    builtins.get(name) match
      case Some(builtin) =>
        builtin(args, env)
      case None =>
        env.ctors.get(name) match
          case Some(arity) =>
            if args.length != arity then
              throw new IllegalArgumentException(s"Constructor $name expects $arity args, got ${args.length}")
            (Value.AdtVal(name, args), env)
          case None =>
            env.functions.get(name) match
              case Some(funDef) =>
                if args.length != funDef.params.length then
                  throw new IllegalArgumentException(
                    s"Function $name expects ${funDef.params.length} args, got ${args.length}"
                  )
                val bindings = funDef.params.zip(args).toMap
                val envWithParams = env.pushScope(bindings)
                try
                  val (result, envAfter) = evalSuite(funDef.body, envWithParams)
                  (result, envAfter.popScope)
                catch
                  case ReturnSignal(value, envAfter) =>
                    (value, envAfter.popScope)
              case None =>
                throw new IllegalArgumentException(s"Unknown function: $name")

  def evalExpr(expr: Expr, env: Env): (Value, Env) =
    expr match
      case Expr.Lit(value) =>
        (literalValue(value), env)
      case Expr.Var(name) =>
        env.resolve(name) match
          case Some(value) => (value, env)
          case None => throw new IllegalArgumentException(s"Unknown variable: $name")
      case Expr.Paren(inner) =>
        evalExpr(inner, env)
      case Expr.Call(callee, args) =>
        callee match
          case Expr.Var(".") if args.length == 2 =>
            val (target, envAfterTarget) = evalExpr(args.head, env)
            val fieldName = args(1) match
              case Expr.Var(name) => name
              case Expr.Lit(Literal.StringLit(value)) => value
              case other => throw new IllegalArgumentException(s"Expected field name, got: $other")
            builtinDot(List(target, Value.StringVal(fieldName)), envAfterTarget)
          case Expr.Var(name) =>
            val (argValues, envAfterArgs) = evalArgs(args, env)
            evalFunc(name, argValues, envAfterArgs)
          case _ =>
            throw new IllegalArgumentException(s"Call target must be a variable, got: $callee")
      case Expr.LetIn(name, _, _, valueExpr, bodyExpr) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        val envWithBinding = envAfterValue.pushScope(Map(name -> value))
        val (result, envAfterBody) = evalExpr(bodyExpr, envWithBinding)
        (result, envAfterBody.popScope)
      case Expr.IfThenElse(cond, thenExpr, elseExpr) =>
        val (condValue, envAfterCond) = evalExpr(cond, env)
        condValue match
          case Value.BoolVal(true) => evalExpr(thenExpr, envAfterCond)
          case Value.BoolVal(false) => evalExpr(elseExpr, envAfterCond)
          case other => throw new IllegalArgumentException(s"Expected Bool in if condition, got: $other")
      case Expr.For(name, inExpr, body) =>
        val (collection, envAfterCollection) = evalExpr(inExpr, env)
        val elements = iterableElements(collection)
        var currentEnv = envAfterCollection
        var lastValue: Value = Value.UnitVal
        elements.foreach { elem =>
          val envWithElem = currentEnv.pushScope(Map(name -> elem))
          val (value, envAfterBody) = evalSuite(body, envWithElem)
          lastValue = value
          currentEnv = envAfterBody.popScope
        }
        (lastValue, currentEnv)
      case Expr.While(cond, body) =>
        @tailrec
        def loop(loopEnv: Env, lastValue: Value): (Value, Env) =
          val (condValue, envAfterCond) = evalExpr(cond, loopEnv)
          condValue match
            case Value.BoolVal(true) =>
              val (value, envAfterBody) = evalSuite(body, envAfterCond)
              loop(envAfterBody, value)
            case Value.BoolVal(false) =>
              (lastValue, envAfterCond)
            case other =>
              throw new IllegalArgumentException(s"Expected Bool in while condition, got: $other")

        loop(env, Value.UnitVal)
      case Expr.Match(scrutinee, cases) =>
        val (value, envAfterScrutinee) = evalExpr(scrutinee, env)
        cases.iterator
          .flatMap { matchCase =>
            matchPattern(matchCase.pattern, value, envAfterScrutinee).map { bindings =>
              val envWithBindings = envAfterScrutinee.pushScope(bindings)
              val (result, envAfterBody) = evalSuite(matchCase.body, envWithBindings)
              (result, envAfterBody.popScope)
            }
          }
          .toSeq
          .headOption
          .getOrElse(throw new IllegalArgumentException(s"No match for value: ${renderValue(value)}"))
      case Expr.Return(valueExpr) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        throw ReturnSignal(value, envAfterValue)

  def evalSuite(suite: Suite, env: Env): (Value, Env) =
    suite match
      case Suite.Single(expr) =>
        evalExpr(expr, env)
      case Suite.Block(exprs) =>
        var currentEnv = env
        var lastValue: Value = Value.UnitVal
        exprs.foreach { expr =>
          val (value, envAfter) = evalExpr(expr, currentEnv)
          lastValue = value
          currentEnv = envAfter
        }
        (lastValue, currentEnv)

  private def evalArgs(args: List[Expr], env: Env): (List[Value], Env) =
    var currentEnv = env
    val values = args.map { arg =>
      val (value, envAfter) = evalExpr(arg, currentEnv)
      currentEnv = envAfter
      value
    }
    (values, currentEnv)

  private def buildEnv(program: ProgramFile): Env =
    var functions = Map.empty[String, FunDef]
    var ctors = Map.empty[String, Int]
    program.items.foreach {
          case TopLevel.DataDecl(_, _, constructors) =>
            constructors.foreach { ctor =>
              ctors = ctors + (ctor.name -> ctor.fields.length)
            }
          case TopLevel.FunDecl(name, _, params, _, body) =>
            functions = functions + (name -> FunDef(params.map(_.name), body))
        }
    Env(List(baseScope), functions, ctors)

  private def baseScope: Map[String, Value] =
    Map(
      "unit" -> Value.UnitVal,
      "undefined" -> Value.UndefinedVal,
      "true" -> Value.BoolVal(true),
      "false" -> Value.BoolVal(false)
    )

  private def literalValue(literal: Literal): Value =
    literal match
      case Literal.IntLit(value) => Value.IntVal(BigInt(value))
      case Literal.BoolLit(value) => Value.BoolVal(value)
      case Literal.StringLit(value) => Value.StringVal(value)

  private def matchPattern(pattern: Pattern, value: Value, env: Env): Option[Map[String, Value]] =
    val res = pattern match
      case Pattern.Wildcard() =>
        Some(Map.empty)
      case Pattern.Lit(literal) =>
        if literalValue(literal) == value then Some(Map.empty) else None
      case Pattern.BinderOrCtor0(name) =>
        env.ctors.get(name) match
          case Some(0) =>
            value match
              case Value.AdtVal(ctorName, args) if ctorName == name && args.isEmpty => Some(Map.empty)
              case _ => None
          case _ =>
            Some(Map(name -> value))
      case Pattern.Ctor(name, args) =>
        value match
          case Value.AdtVal(ctorName, ctorArgs) if ctorName == name && ctorArgs.length == args.length =>
            matchPatternList(args, ctorArgs, env)
          case _ => None
    res

  private def matchPatternList(
      patterns: List[Pattern],
      values: List[Value],
      env: Env
    ): Option[Map[String, Value]] =
    val results = patterns.zip(values).map { case (pat, v) => matchPattern(pat, v, env) }
    if results.forall(_.isDefined) then
      Some(results.flatten.foldLeft(Map.empty[String, Value])(_ ++ _))
    else
      None

  private def iterableElements(value: Value): List[Value] =
    value match
      case Value.ListVal(values) => values.toList
      case Value.SetVal(values) => values.toList
      case Value.MapVal(values) => values.keys.toList
      case other =>
        throw new IllegalArgumentException(s"Expected iterable, got: $other")

  private def builtins: Map[String, Builtin] =
    Map(
      "println" -> builtinPrintln,
      "+" -> builtinPlus,
      "-" -> builtinMinus,
      "*" -> builtinMul,
      "/" -> builtinDiv,
      "%" -> builtinMod,
      "<" -> builtinCompare(_ < 0),
      "<=" -> builtinCompare(_ <= 0),
      ">" -> builtinCompare(_ > 0),
      ">=" -> builtinCompare(_ >= 0),
      "==" -> builtinEq(_ == _),
      "!=" -> builtinEq(_ != _),
      "and" -> builtinBool(_ && _),
      "or" -> builtinBool(_ || _),
      "list" -> builtinList,
      "set" -> builtinSet,
      "map" -> builtinMap,
      "len" -> builtinLen,
      "get" -> builtinGet,
      "put" -> builtinPut,
      "add" -> builtinAdd,
      "remove" -> builtinRemove,
      "contains" -> builtinContains,
      "keys" -> builtinKeys,
      "values" -> builtinValues,
      "append" -> builtinAppend,
      "concat" -> builtinConcat,
      "slice" -> builtinSlice,
      "block" -> builtinBlock
    )

  private def builtinPrintln(args: List[Value], env: Env): (Value, Env) =
    // TODO do not use global state, pass in the PrintStream via the Env
    args.foreach(value => System.out.println(renderValue(value)))
    (Value.UnitVal, env)

  private def builtinPlus(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.IntVal(a), Value.IntVal(b)) => (Value.IntVal(a + b), env)
      case List(Value.StringVal(a), Value.StringVal(b)) => (Value.StringVal(a + b), env)
      case List(Value.ListVal(a), Value.ListVal(b)) => (Value.ListVal(a ++ b), env)
      case other => throw new IllegalArgumentException(s"Unsupported + args: $other")

  private def builtinMinus(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.IntVal(a)) => (Value.IntVal(-a), env)
      case List(Value.IntVal(a), Value.IntVal(b)) => (Value.IntVal(a - b), env)
      case other => throw new IllegalArgumentException(s"Unsupported - args: $other")

  private def builtinMul(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.IntVal(a), Value.IntVal(b)) => (Value.IntVal(a * b), env)
      case other => throw new IllegalArgumentException(s"Unsupported * args: $other")

  private def builtinDiv(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.IntVal(a), Value.IntVal(b)) => (Value.IntVal(a / b), env)
      case other => throw new IllegalArgumentException(s"Unsupported / args: $other")

  private def builtinMod(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.IntVal(a), Value.IntVal(b)) => (Value.IntVal(a % b), env)
      case other => throw new IllegalArgumentException(s"Unsupported % args: $other")

  private def builtinCompare(pred: Int => Boolean): Builtin =
    (args, env) =>
      args match
        case List(Value.IntVal(a), Value.IntVal(b)) => (Value.BoolVal(pred(a.compare(b))), env)
        case List(Value.StringVal(a), Value.StringVal(b)) =>
          (Value.BoolVal(pred(a.compareTo(b))), env)
        case other => throw new IllegalArgumentException(s"Unsupported comparison args: $other")

  private def builtinEq(pred: (Value, Value) => Boolean): Builtin =
    (args, env) =>
      args match
        case List(a, b) => (Value.BoolVal(pred(a, b)), env)
        case other => throw new IllegalArgumentException(s"Unsupported equality args: $other")

  private def builtinBool(op: (Boolean, Boolean) => Boolean): Builtin =
    (args, env) =>
      args match
        case List(Value.BoolVal(a), Value.BoolVal(b)) => (Value.BoolVal(op(a, b)), env)
        case other => throw new IllegalArgumentException(s"Unsupported boolean args: $other")

  private def builtinList(args: List[Value], env: Env): (Value, Env) =
    (Value.ListVal(args.toVector), env)

  private def builtinSet(args: List[Value], env: Env): (Value, Env) =
    (Value.SetVal(args.toSet), env)

  private def builtinMap(args: List[Value], env: Env): (Value, Env) =
    if args.length % 2 != 0 then
      throw new IllegalArgumentException("map expects an even number of args (key/value pairs)")
    val entries = args.grouped(2).map(listToPair).toMap
    (Value.MapVal(entries), env)

  private def listToPair(list: List[Value]): (Value, Value) =
    list match
      case List(a, b) => (a, b)
      case other => throw new IllegalArgumentException(s"Expected list of length 2 for pair, got: $other")

  private def builtinLen(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.ListVal(values)) => (Value.IntVal(values.length), env)
      case List(Value.SetVal(values)) => (Value.IntVal(values.size), env)
      case List(Value.MapVal(values)) => (Value.IntVal(values.size), env)
      case List(Value.StringVal(value)) => (Value.IntVal(value.length), env)
      case other => throw new IllegalArgumentException(s"Unsupported len args: $other")

  private def builtinGet(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.ListVal(values), Value.IntVal(index)) =>
        val i = index.toInt
        if i < 0 || i >= values.length then
          throw new IllegalArgumentException(s"List index out of bounds: $i")
        (values(i), env)
      case List(Value.MapVal(values), key) =>
        values.get(key) match
          case Some(value) => (value, env)
          case None => throw new IllegalArgumentException(s"Missing key: ${renderValue(key)}")
      case other => throw new IllegalArgumentException(s"Unsupported get args: $other")

  private def builtinPut(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values), key, value) =>
        (Value.MapVal(values + (key -> value)), env)
      case other => throw new IllegalArgumentException(s"Unsupported put args: $other")

  private def builtinAdd(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.SetVal(values), value) =>
        (Value.SetVal(values + value), env)
      case other => throw new IllegalArgumentException(s"Unsupported add args: $other")

  private def builtinRemove(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.SetVal(values), value) =>
        (Value.SetVal(values - value), env)
      case other => throw new IllegalArgumentException(s"Unsupported remove args: $other")

  private def builtinContains(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.SetVal(values), value) =>
        (Value.BoolVal(values.contains(value)), env)
      case List(Value.ListVal(values), value) =>
        (Value.BoolVal(values.contains(value)), env)
      case List(Value.MapVal(values), key) =>
        (Value.BoolVal(values.contains(key)), env)
      case other => throw new IllegalArgumentException(s"Unsupported contains args: $other")

  private def builtinKeys(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values)) => (Value.ListVal(values.keys.toVector), env)
      case other => throw new IllegalArgumentException(s"Unsupported keys args: $other")

  private def builtinValues(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values)) => (Value.ListVal(values.values.toVector), env)
      case other => throw new IllegalArgumentException(s"Unsupported values args: $other")

  private def builtinAppend(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.ListVal(values), value) => (Value.ListVal(values :+ value), env)
      case other => throw new IllegalArgumentException(s"Unsupported append args: $other")

  private def builtinConcat(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.ListVal(a), Value.ListVal(b)) => (Value.ListVal(a ++ b), env)
      case other => throw new IllegalArgumentException(s"Unsupported concat args: $other")

  private def builtinSlice(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.ListVal(values), Value.IntVal(start), Value.IntVal(end)) =>
        val s = start.toInt
        val e = end.toInt
        (Value.ListVal(values.slice(s, e)), env)
      case other => throw new IllegalArgumentException(s"Unsupported slice args: $other")

  private def builtinBlock(args: List[Value], env: Env): (Value, Env) =
    val value = args.lastOption.getOrElse(Value.UnitVal)
    (value, env)

  private def builtinDot(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values), Value.StringVal(field)) =>
        values.get(Value.StringVal(field)) match
          case Some(value) => (value, env)
          case None => throw new IllegalArgumentException(s"Missing map field: $field")
      case other => throw new IllegalArgumentException(s"Unsupported . args: $other")

  private def renderValue(value: Value): String =
    value match
      case Value.IntVal(v) => v.toString
      case Value.BoolVal(v) => v.toString
      case Value.StringVal(v) => v
      case Value.ListVal(values) => values.map(renderValue).mkString("[", ", ", "]")
      case Value.SetVal(values) => values.map(renderValue).mkString("Set(", ", ", ")")
      case Value.MapVal(values) =>
        values.map { case (k, v) => s"${renderValue(k)}: ${renderValue(v)}" }.mkString("Map(", ", ", ")")
      case Value.AdtVal(name, args) =>
        if args.isEmpty then name else s"$name${args.map(renderValue).mkString("(", ", ", ")")}"
      case Value.UnitVal => "unit"
      case Value.UndefinedVal => "undefined"
      case Value.FuncVal(_) => "<function>"

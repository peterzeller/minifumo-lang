package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.ast.Literal
import com.github.peterzeller.minifumo.typing.TypedAst.*

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
    case FuncVal(
        name: String,
        arity: Int,
        captured: List[Value],
        fn: (List[Value], Env) => (Value, Env)
      )
    case UndefinedVal

    override def toString(): String =
      renderValue(this)

  final case class FunDef(params: List[ParamSymbol], body: Expr)

  final case class Env(
      scopes: List[Map[TermSymbol, Value]],
      functions: Map[FunctionSymbol, FunDef],
      ctors: Map[CtorSymbol, Int],
      eliminators: Map[String, List[CtorSymbol]]
    ):
    def resolve(symbol: TermSymbol): Option[Value] =
      scopes.collectFirst { case scope if scope.contains(symbol) => scope(symbol) }.orElse(baseValue(symbol))

    private def baseValue(symbol: TermSymbol): Option[Value] =
      symbol match
        case BuiltinValueSymbol(name, _) => Interpreter.baseValues.get(name)
        case _ => None

    def pushScope(bindings: Map[TermSymbol, Value]): Env =
      copy(scopes = bindings :: scopes)

    def popScope: Env =
      copy(scopes = scopes.drop(1))

    def withBinding(symbol: TermSymbol, value: Value): Env =
      scopes match
        case head :: tail => copy(scopes = (head + (symbol -> value)) :: tail)
        case Nil => copy(scopes = List(Map(symbol -> value)))

    def updateBinding(symbol: TermSymbol, value: Value): Env =
      def updateScopes(remaining: List[Map[TermSymbol, Value]]): (List[Map[TermSymbol, Value]], Boolean) =
        remaining match
          case Nil => (Nil, false)
          case head :: tail =>
            if head.contains(symbol) then
              ((head + (symbol -> value)) :: tail, true)
            else
              val (updatedTail, found) = updateScopes(tail)
              (head :: updatedTail, found)
      val (updatedScopes, found) = updateScopes(scopes)
      if !found then
        throw new IllegalArgumentException(s"Unknown variable: ${symbol.name}")
      copy(scopes = updatedScopes)

  private final case class ReturnSignal(value: Value, env: Env) extends RuntimeException with NoStackTrace

  type Builtin = (List[Value], Env) => (Value, Env)
  final case class BuiltinDef(arity: Int, fn: Builtin)

  def evalProg(program: Program, entryName: String): Value =
    val env = buildEnv(program)
    val entrySymbol = program.items.collectFirst { case TopLevel.FunDecl(symbol, _, _, _) if symbol.name == entryName => symbol }
      .getOrElse(throw new IllegalArgumentException(s"Unknown function: $entryName"))
    val (value, _) = evalFunc(entrySymbol, Nil, env)
    value

  def evalFunc(symbol: Symbol, args: List[Value], env: Env): (Value, Env) =
    try
      symbol match
        case ctor: CtorSymbol =>
          if args.length != ctor.arity then
            throw new IllegalArgumentException(s"Constructor ${ctor.name} expects ${ctor.arity} args, got ${args.length}")
          (Value.AdtVal(ctor.name, args), env)
        case fun: FunctionSymbol =>
          builtins.get(fun.name) match
            case Some(BuiltinDef(_, builtin)) =>
              builtin(args, env)
            case None =>
              env.functions.get(fun) match
                case Some(funDef) =>
                  if args.length != funDef.params.length then
                    throw new IllegalArgumentException(
                      s"Function ${fun.name} expects ${funDef.params.length} args, got ${args.length}"
                    )
                  val bindings: Map[TermSymbol, Value] = funDef.params.zip(args).toMap
                  val envWithParams = env.pushScope(bindings)
                  try
                    val (result, envAfter) = evalExpr(funDef.body, envWithParams)
                    (result, envAfter.popScope)
                  catch
                    case ReturnSignal(value, envAfter) =>
                      (value, envAfter.popScope)
                case None =>
                  throw new IllegalArgumentException(s"Unknown function: ${fun.name}")
        case err: ErrorSymbol =>
          throw new IllegalArgumentException(s"Unknown symbol: ${err.name}")
        case term: TermSymbol =>
          throw new IllegalArgumentException(s"Expected function symbol, got term: ${term.name}")
    catch
      case e: Exception =>
        throw new Exception(s"In function ${symbol.name}:\n${e.getMessage}", e)

  def evalExpr(expr: Expr, env: Env): (Value, Env) =
    expr match
      case Expr.Lit(value, _) =>
        (literalValue(value), env)
      case Expr.Var(symbol, _) =>
        val value = symbol match
          case term: TermSymbol =>
            env.resolve(term).getOrElse(throw new IllegalArgumentException(s"Unknown variable: ${term.name}"))
          case fun: FunctionSymbol =>
            builtins.get(fun.name) match
              case Some(BuiltinDef(arity, builtin)) =>
                // Override function with builtin implementation
                Value.FuncVal(fun.name, arity, Nil, builtin)
              case None =>
                env.eliminators.get(fun.name) match
                  case Some(ctors) =>
                    Value.FuncVal(fun.name, ctors.length + 1, Nil, buildEliminator(ctors))
                  case None =>
                    Value.FuncVal(
                      fun.name,
                      fun.tpe.params.length,
                      Nil,
                      (args, env) => evalFunc(fun, args, env)
                    )
          case ctor: CtorSymbol =>
            if ctor.arity == 0 then Value.AdtVal(ctor.name, Nil)
            else
              Value.FuncVal(
                ctor.name,
                ctor.arity,
                Nil,
                (args: List[Value], env: Env) => evalFunc(ctor, args, env)
              )
          case err: ErrorSymbol =>
            throw new IllegalArgumentException(s"Unknown symbol: ${err.name}")
        (value, env)
      case Expr.Paren(inner, _) =>
        evalExpr(inner, env)
      case Expr.CallFun(callee, args, _) =>
        val (calleeValue, envAfterCallee) = evalExpr(callee, env)
        val (argValues, envAfterArgs) = evalArgs(args, envAfterCallee)
        applyFunctionValue(calleeValue, argValues, envAfterArgs)
      case Expr.CallCtor(symbol, args, _) =>
        val (argValues, envAfterArgs) = evalArgs(args, env)
        if argValues.length > symbol.arity then
          throw new IllegalArgumentException(
            s"Constructor ${symbol.name} expects at most ${symbol.arity} args, got ${argValues.length}"
          )
        if argValues.length == symbol.arity then
          (Value.AdtVal(symbol.name, argValues), envAfterArgs)
        else
          val remaining = symbol.arity - argValues.length
          val fn = (allArgs: List[Value], env: Env) =>
            if allArgs.length != symbol.arity then
              throw new IllegalArgumentException(
                s"Constructor ${symbol.name} expects ${symbol.arity} args, got ${allArgs.length}"
              )
            (Value.AdtVal(symbol.name, allArgs), env)
          (Value.FuncVal(symbol.name, remaining, argValues, fn), envAfterArgs)
      case Expr.Lambda(param, body, _) =>
        val capturedEnv = env
        val fn = (args: List[Value], _: Env) =>
          args match
            case List(value) =>
              val envWithParam = capturedEnv.pushScope(Map(param -> value))
              val (result, envAfter) = evalExpr(body, envWithParam)
              (result, envAfter.popScope)
            case other =>
              throw new IllegalArgumentException(s"Lambda expects 1 arg, got ${other.length}")
        (Value.FuncVal("<lambda>", 1, Nil, fn), env)
      case Expr.LetIn(symbol, _, _, valueExpr, bodyExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        val envWithBinding = envAfterValue.pushScope(Map(symbol -> value))
        val (result, envAfterBody) = evalExpr(bodyExpr, envWithBinding)
        (result, envAfterBody.popScope)
      case Expr.Bind(symbol, _, _, valueExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        (Value.UnitVal, envAfterValue.withBinding(symbol, value))
      case Expr.Return(valueExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        throw ReturnSignal(value, envAfterValue)

  private def evalArgs(args: List[Expr], env: Env): (List[Value], Env) =
    var currentEnv = env
    val values = args.map { arg =>
      val (value, envAfter) = evalExpr(arg, currentEnv)
      currentEnv = envAfter
      value
    }
    (values, currentEnv)

  // Applies a curried function value to explicit arguments in order.
  private def applyFunctionValue(
      value: Value,
      explicitArgs: List[Value],
      env: Env
    ): (Value, Env) =
    value match
      case Value.FuncVal(_, 0, captured, fn) if explicitArgs.isEmpty =>
        fn(captured, env)
      case _ =>
        val (afterExplicit, envAfterExplicit) =
          explicitArgs.foldLeft((value, env)) { case ((currentValue, currentEnv), arg) =>
            applyExplicitArg(currentValue, arg, currentEnv)
          }
        afterExplicit match
          case Value.FuncVal(_, 0, captured, fn) if captured.nonEmpty =>
            fn(captured, envAfterExplicit)
          case _ =>
            (afterExplicit, envAfterExplicit)

  // Applies an explicit argument to a function value.
  private def applyExplicitArg(value: Value, arg: Value, env: Env): (Value, Env) =
    value match
      case Value.FuncVal(name, arity, captured, fn) =>
        if arity <= 0 then
          throw new IllegalArgumentException(s"Function $name expects no more explicit args")
        val updatedCaptured = captured :+ arg
        val remaining = arity - 1
        val updatedValue = Value.FuncVal(name, remaining, updatedCaptured, fn)
        if remaining == 0 then
          fn(updatedCaptured, env)
        else
          (updatedValue, env)
      case other =>
        throw new IllegalArgumentException(s"Call target must be a function, got: $other")

  // Builds an eliminator function implementation for a data type's constructors.
  private def buildEliminator(ctors: List[CtorSymbol]): Builtin =
    (args: List[Value], env: Env) =>
      if args.length != ctors.length + 1 then
        throw new IllegalArgumentException(
          s"Eliminator expects ${ctors.length + 1} args, got ${args.length}"
        )
      val scrutinee = args.head
      val handlers = args.tail
      scrutinee match
        case Value.AdtVal(ctorName, ctorArgs) =>
          val index = ctors.indexWhere(_.name == ctorName)
          if index < 0 then
            throw new IllegalArgumentException(s"Unknown constructor in eliminator: $ctorName")
          val handler = handlers(index)
          val argsToApply = if ctorArgs.isEmpty then List(Value.UnitVal) else ctorArgs
          applyFunctionValue(handler, argsToApply, env)
        case other =>
          throw new IllegalArgumentException(s"Eliminator expects an ADT value, got: $other")

  // Builds the implicit eliminator name for a data type.
  private def buildEliminatorName(typeName: String): String =
    s"${typeName}_elim"

  private def buildEnv(program: Program): Env =
    var functions = Map.empty[FunctionSymbol, FunDef]
    var ctors = Map.empty[CtorSymbol, Int]
    var eliminators = Map.empty[String, List[CtorSymbol]]
    program.items.foreach {
          case TopLevel.DataDecl(typeName, _, constructors) =>
            constructors.foreach { ctor =>
              ctors = ctors + (ctor.symbol -> ctor.symbol.arity)
            }
            val ctorList = constructors.map(_.symbol)
            eliminators = eliminators + (buildEliminatorName(typeName) -> ctorList)
          case TopLevel.FunDecl(symbol, _, params, body) =>
            functions = functions + (symbol -> FunDef(params, body))
        }
    Env(List(Map.empty), functions, ctors, eliminators)

  // Wraps a boolean value as a Bool constructor value.
  private def boolToValue(value: Boolean): Value =
    if value then Value.AdtVal("True", Nil) else Value.AdtVal("False", Nil)

  // Extracts a boolean value from Bool constructors or runtime booleans.
  private def valueToBool(value: Value): Option[Boolean] =
    value match
      case Value.BoolVal(v) => Some(v)
      case Value.AdtVal("True", Nil) => Some(true)
      case Value.AdtVal("False", Nil) => Some(false)
      case _ => None

  // Wraps a natural number as a Nat constructor value.
  private def natToValue(value: BigInt): Value =
    if value <= 0 then Value.AdtVal("Zero", Nil)
    else Value.AdtVal("Suc", List(natToValue(value - 1)))

  // Extracts a natural number from Nat constructor values.
  private def valueToNat(value: Value): Option[BigInt] =
    value match
      case Value.AdtVal("Zero", Nil) => Some(0)
      case Value.AdtVal("Suc", List(rest)) => valueToNat(rest).map(_ + 1)
      case _ => None

  // Wraps an integer as an Int constructor value.
  private def intToValue(value: BigInt): Value =
    val positive = value >= 0
    val magnitude = value.abs
    Value.AdtVal("Int", List(boolToValue(positive), natToValue(magnitude)))

  // Extracts an integer from Int constructor values or runtime integers.
  private def valueToInt(value: Value): Option[BigInt] =
    value match
      case Value.IntVal(v) => Some(v)
      case Value.AdtVal("Int", List(signValue, natValue)) =>
        for
          sign <- valueToBool(signValue)
          magnitude <- valueToNat(natValue)
        yield if sign then magnitude else -magnitude
      case _ => None

  // Wraps a list of values as a List constructor chain.
  private def listToValue(values: Vector[Value]): Value =
    values.foldRight(Value.AdtVal("Nil", Nil)) { (value, acc) =>
      Value.AdtVal("Cons", List(value, acc))
    }

  // Extracts a list of values from List constructors or runtime lists.
  private def valueToList(value: Value): Option[Vector[Value]] =
    value match
      case Value.ListVal(values) => Some(values)
      case Value.AdtVal("Nil", Nil) => Some(Vector.empty)
      case Value.AdtVal("Cons", List(head, tail)) =>
        valueToList(tail).map(rest => head +: rest)
      case _ => None

  // Wraps a code point as a Char constructor value.
  private def charToValue(codePoint: Int): Value =
    Value.AdtVal("Char", List(natToValue(BigInt(codePoint))))

  // Extracts a code point from Char constructor values.
  private def valueToChar(value: Value): Option[Int] =
    value match
      case Value.AdtVal("Char", List(runeValue)) =>
        valueToNat(runeValue).map(_.toInt)
      case _ => None

  // Wraps a JVM string as a String constructor value.
  private def stringToValue(value: String): Value =
    val chars = value.toVector.map(ch => charToValue(ch.toInt))
    Value.AdtVal("String", List(listToValue(chars)))

  // Extracts a JVM string from String constructors or runtime strings.
  private def valueToString(value: Value): Option[String] =
    value match
      case Value.StringVal(v) => Some(v)
      case Value.AdtVal("String", List(charsValue)) =>
        valueToList(charsValue).flatMap { chars =>
          val decoded = chars.map(valueToChar)
          if decoded.forall(_.isDefined) then
            Some(decoded.flatten.map(_.toChar).mkString)
          else
            None
        }
      case _ => None

  private val baseValues: Map[String, Value] =
    Map(
      "unit" -> Value.UnitVal,
      "undefined" -> Value.UndefinedVal
    )

  private def literalValue(literal: Literal): Value =
    literal match
      case Literal.IntLit(value) => intToValue(BigInt(value))
      case Literal.BoolLit(value) => boolToValue(value)
      case Literal.StringLit(value) => stringToValue(value)
      case Literal.UnitLit() => Value.UnitVal

  private def builtins: Map[String, BuiltinDef] =
    Map(
      "printlnString" -> BuiltinDef(1, builtinPrintln),
      "natToString" -> BuiltinDef(1, builtinNatToString),
      "intToString" -> BuiltinDef(1, builtinIntToString),
      "opPlus" -> BuiltinDef(2, builtinIntAdd),
      "opMinus" -> BuiltinDef(2, builtinIntSub),
      "opLt" -> BuiltinDef(2, builtinIntLt),
      "opLe" -> BuiltinDef(2, builtinIntLe)
    )

  private def builtinPrintln(args: List[Value], env: Env): (Value, Env) =
    // TODO do not use global state, pass in the PrintStream via the Env
    args match
      case List(value) =>
        val text = valueToString(value).getOrElse(renderValue(value))
        System.out.println(text)
        (Value.UnitVal, env)
      case other =>
        throw new IllegalArgumentException(s"printlnString expects 1 arg, got: $other")

  // Converts a Nat value to a String constructor.
  private def builtinNatToString(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(value) =>
        valueToNat(value) match
          case Some(nat) => (stringToValue(nat.toString), env)
          case None => throw new IllegalArgumentException(s"natToString expects Nat, got: $value")
      case other =>
        throw new IllegalArgumentException(s"natToString expects 1 arg, got: $other")

  // Converts an Int value to a String constructor.
  private def builtinIntToString(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(value) =>
        valueToInt(value) match
          case Some(intValue) => (stringToValue(intValue.toString), env)
          case None => throw new IllegalArgumentException(s"intToString expects Int, got: $value")
      case other =>
        throw new IllegalArgumentException(s"intToString expects 1 arg, got: $other")

  // Adds two Int values using host arithmetic.
  private def builtinIntAdd(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(left, right) =>
        (valueToInt(left), valueToInt(right)) match
          case (Some(a), Some(b)) => (intToValue(a + b), env)
          case _ => throw new IllegalArgumentException(s"opPlus expects Int args, got: $left, $right")
      case other =>
        throw new IllegalArgumentException(s"opPlus expects 2 args, got: $other")

  // Subtracts two Int values using host arithmetic.
  private def builtinIntSub(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(left, right) =>
        (valueToInt(left), valueToInt(right)) match
          case (Some(a), Some(b)) => (intToValue(a - b), env)
          case _ => throw new IllegalArgumentException(s"opMinus expects Int args, got: $left, $right")
      case other =>
        throw new IllegalArgumentException(s"opMinus expects 2 args, got: $other")

  // Compares two Int values for strictly-less-than.
  private def builtinIntLt(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(left, right) =>
        (valueToInt(left), valueToInt(right)) match
          case (Some(a), Some(b)) => (boolToValue(a < b), env)
          case _ => throw new IllegalArgumentException(s"opLt expects Int args, got: $left, $right")
      case other =>
        throw new IllegalArgumentException(s"opLt expects 2 args, got: $other")

  // Compares two Int values for less-than-or-equal.
  private def builtinIntLe(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(left, right) =>
        (valueToInt(left), valueToInt(right)) match
          case (Some(a), Some(b)) => (boolToValue(a <= b), env)
          case _ => throw new IllegalArgumentException(s"opLe expects Int args, got: $left, $right")
      case other =>
        throw new IllegalArgumentException(s"opLe expects 2 args, got: $other")

  private def renderValue(value: Value): String =
    valueToInt(value)
      .map(_.toString)
      .orElse(valueToBool(value).map(_.toString))
      .orElse(valueToString(value))
      .orElse(valueToChar(value).map(code => s"'${code.toChar}'"))
      .getOrElse {
        value match
          case Value.SetVal(values) => values.map(renderValue).mkString("Set(", ", ", ")")
          case Value.MapVal(values) =>
            values.map { case (k, v) => s"${renderValue(k)}: ${renderValue(v)}" }.mkString("Map(", ", ", ")")
          case Value.AdtVal(name, args) =>
            if args.isEmpty then name else s"$name${args.map(renderValue).mkString("(", ", ", ")")}"
          case Value.UnitVal => "unit"
          case Value.UndefinedVal => "undefined"
          case Value.FuncVal(name, _, _, _) => s"<function $name>"
          case Value.IntVal(v) => v.toString
          case Value.BoolVal(v) => v.toString
          case Value.StringVal(v) => v
          case Value.ListVal(values) => values.map(renderValue).mkString("[", ", ", "]")
      }

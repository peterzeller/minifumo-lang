package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.ast.Literal
import com.github.peterzeller.minifumo.typing.TypedAst.*

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
    // Maps member names to their implementation plus any captured given arguments.
    case InstanceVal(members: Map[String, InstanceMemberValue])
    case UnitVal
    case FuncVal(fn: (List[Value], Env) => (Value, Env))
    case UndefinedVal

    override def toString(): String =
      renderValue(this)

  final case class FunDef(params: List[ParamSymbol], body: Suite)

  // Captured holds resolved given arguments for the instance member.
  final case class InstanceMemberValue(symbol: FunctionSymbol, captured: List[Value])

  final case class Env(
      scopes: List[Map[TermSymbol, Value]],
      functions: Map[FunctionSymbol, FunDef],
      ctors: Map[CtorSymbol, Int],
      instances: Map[InstanceSymbol, TopLevel.InstanceDecl]
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

  def evalProg(program: Program, entryName: String): Value =
    val env = buildEnv(program)
    val entrySymbol = program.items.collectFirst { case TopLevel.FunDecl(symbol, _, _, _, _) if symbol.name == entryName => symbol }
      .getOrElse(throw new IllegalArgumentException(s"Unknown function: $entryName"))
    val (value, _) = evalFunc(entrySymbol, Nil, env)
    value

  def evalFunc(symbol: Symbol, args: List[Value], env: Env): (Value, Env) =
    symbol match
      case builtin: BuiltinFunctionSymbol =>
        builtins.get(builtin.name) match
          case Some(fn) => fn(args, env)
          case None => throw new IllegalArgumentException(s"Unknown builtin: ${builtin.name}")
      case ctor: CtorSymbol =>
        if args.length != ctor.arity then
          throw new IllegalArgumentException(s"Constructor ${ctor.name} expects ${ctor.arity} args, got ${args.length}")
        (Value.AdtVal(ctor.name, args), env)
      case fun: FunctionSymbol =>
        if fun.name == "println" then
          builtinPrintln(args, env)
        else
          env.functions.get(fun) match
            case Some(funDef) =>
              if args.length != funDef.params.length then
                throw new IllegalArgumentException(
                  s"Function ${fun.name} expects ${funDef.params.length} args, got ${args.length}"
                )
              val bindings: Map[TermSymbol, Value] = funDef.params.zip(args).toMap
              val envWithParams = env.pushScope(bindings)
              try
                val (result, envAfter) = evalSuite(funDef.body, envWithParams)
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

  def evalExpr(expr: Expr, env: Env): (Value, Env) =
    expr match
      case Expr.Lit(value, _) =>
        (literalValue(value), env)
      case Expr.Var(symbol, _) =>
        val value = symbol match
          case instance: InstanceSymbol =>
            env.instances.get(instance) match
              case Some(instanceDecl) if instance.givenParams.isEmpty =>
                val members = instanceDecl.members.map { member =>
                  member.memberName -> InstanceMemberValue(member.symbol, Nil)
                }.toMap
                Value.InstanceVal(members)
              case Some(_) =>
                throw new IllegalArgumentException(s"Instance ${instance.name} requires given arguments")
              case None =>
                throw new IllegalArgumentException(s"Unknown instance: ${instance.name}")
          case term: TermSymbol =>
            env.resolve(term).getOrElse(throw new IllegalArgumentException(s"Unknown variable: ${term.name}"))
          case fun: FunctionSymbol =>
            Value.FuncVal((args: List[Value], env: Env) => evalFunc(fun, args, env))
          case ctor: CtorSymbol =>
            if ctor.arity == 0 then Value.AdtVal(ctor.name, Nil)
            else Value.FuncVal((args: List[Value], env: Env) => evalFunc(ctor, args, env))
          case builtin: BuiltinFunctionSymbol =>
            Value.FuncVal((args: List[Value], env: Env) => evalFunc(builtin, args, env))
          case err: ErrorSymbol =>
            throw new IllegalArgumentException(s"Unknown symbol: ${err.name}")
        (value, env)
      case Expr.Paren(inner, _) =>
        evalExpr(inner, env)
      case Expr.Block(exprs, _) =>
        var currentEnv = env
        var lastValue: Value = Value.UnitVal
        exprs.foreach { expr =>
          val (value, envAfter) = evalExpr(expr, currentEnv)
          lastValue = value
          currentEnv = envAfter
        }
        (lastValue, currentEnv)
      case Expr.CallFun(callee, args, givenArgs, _) =>
        callee match
          case Expr.Var(symbol, _) if symbol.name == "." && args.length == 2 =>
            val (target, envAfterTarget) = evalExpr(args.head, env)
            val fieldName = args(1) match
              case Expr.Var(fieldSymbol, _) => fieldSymbol.name
              case Expr.Lit(Literal.StringLit(value), _) => value
              case other => throw new IllegalArgumentException(s"Expected field name, got: $other")
            builtinDot(List(target, stringToValue(fieldName)), envAfterTarget)
          case _ =>
            val (calleeValue, envAfterCallee) = evalExpr(callee, env)
            val (argValues, envAfterArgs) = evalArgs(args, envAfterCallee)
            val (givenValues, envAfterGivens) = evalArgs(givenArgs, envAfterArgs)
            calleeValue match
              case Value.FuncVal(fn) => fn(argValues ++ givenValues, envAfterGivens)
              case other => throw new IllegalArgumentException(s"Call target must be a function, got: $other")
      case Expr.CallCtor(symbol, args, _) =>
        val (argValues, envAfterArgs) = evalArgs(args, env)
        if argValues.length != symbol.arity then
          throw new IllegalArgumentException(
            s"Constructor ${symbol.name} expects ${symbol.arity} args, got ${argValues.length}"
          )
        (Value.AdtVal(symbol.name, argValues), envAfterArgs)
      case Expr.InstanceValue(symbol, givenArgs, _) =>
        env.instances.get(symbol) match
          case Some(instanceDecl) =>
            val (givenValues, envAfterGivens) = evalArgs(givenArgs, env)
            val members = instanceDecl.members.map { member =>
              member.memberName -> InstanceMemberValue(member.symbol, givenValues)
            }.toMap
            (Value.InstanceVal(members), envAfterGivens)
          case None =>
            throw new IllegalArgumentException(s"Unknown instance: ${symbol.name}")
      case Expr.CallTypeClassMember(instanceExpr, memberName, args, _) =>
        val (instanceValue, envAfterInstance) = evalExpr(instanceExpr, env)
        val (argValues, envAfterArgs) = evalArgs(args, envAfterInstance)
        instanceValue match
          case Value.InstanceVal(members) =>
            members.get(memberName) match
              case Some(InstanceMemberValue(memberSymbol, captured)) =>
                evalFunc(memberSymbol, argValues ++ captured, envAfterArgs)
              case None =>
                throw new IllegalArgumentException(s"Unknown instance member: $memberName")
          case other =>
            throw new IllegalArgumentException(s"Expected typeclass instance, got: $other")
      case Expr.LetIn(symbol, _, _, valueExpr, bodyExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        val envWithBinding = envAfterValue.pushScope(Map(symbol -> value))
        val (result, envAfterBody) = evalExpr(bodyExpr, envWithBinding)
        (result, envAfterBody.popScope)
      case Expr.Bind(symbol, _, _, valueExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        (Value.UnitVal, envAfterValue.withBinding(symbol, value))
      case Expr.Assign(symbol, valueExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        (Value.UnitVal, envAfterValue.updateBinding(symbol, value))
      case Expr.IfThenElse(cond, thenExpr, elseExpr, _) =>
        val (condValue, envAfterCond) = evalExpr(cond, env)
        valueToBool(condValue) match
          case Some(true) => evalExpr(thenExpr, envAfterCond)
          case Some(false) => evalExpr(elseExpr, envAfterCond)
          case None => throw new IllegalArgumentException(s"Expected Bool in if condition, got: $condValue")
      case Expr.For(symbol, inExpr, body, _) =>
        val (collection, envAfterCollection) = evalExpr(inExpr, env)
        val elements = iterableElements(collection)
        var currentEnv = envAfterCollection
        var lastValue: Value = Value.UnitVal
        elements.foreach { elem =>
          val envWithElem = currentEnv.pushScope(Map(symbol -> elem))
          val (value, envAfterBody) = evalSuite(body, envWithElem)
          lastValue = value
          currentEnv = envAfterBody.popScope
        }
        (lastValue, currentEnv)
      case Expr.While(cond, body, _) =>
        @tailrec
        def loop(loopEnv: Env, lastValue: Value): (Value, Env) =
          val (condValue, envAfterCond) = evalExpr(cond, loopEnv)
          valueToBool(condValue) match
            case Some(true) =>
              val (value, envAfterBody) = evalSuite(body, envAfterCond)
              loop(envAfterBody, value)
            case Some(false) =>
              (lastValue, envAfterCond)
            case None =>
              throw new IllegalArgumentException(s"Expected Bool in while condition, got: $condValue")

        loop(env, Value.UnitVal)
      case Expr.Match(scrutinee, cases, _) =>
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
      case Expr.Return(valueExpr, _) =>
        val (value, envAfterValue) = evalExpr(valueExpr, env)
        throw ReturnSignal(value, envAfterValue)

  def evalSuite(suite: Suite, env: Env): (Value, Env) =
    suite match
      case Suite.Single(expr) =>
        evalExpr(expr, env)
      case Suite.Block(exprs, _) =>
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

  private def buildEnv(program: Program): Env =
    var functions = Map.empty[FunctionSymbol, FunDef]
    var ctors = Map.empty[CtorSymbol, Int]
    var instances = Map.empty[InstanceSymbol, TopLevel.InstanceDecl]
    program.items.foreach {
          case TopLevel.DataDecl(_, _, constructors) =>
            constructors.foreach { ctor =>
              ctors = ctors + (ctor.symbol -> ctor.symbol.arity)
            }
          case TopLevel.FunDecl(symbol, _, params, givenParams, body) =>
            functions = functions + (symbol -> FunDef(params ++ givenParams, body))
          case instanceDecl: TopLevel.InstanceDecl =>
            instances = instances + (instanceDecl.symbol -> instanceDecl)
            instanceDecl.members.foreach { member =>
              functions = functions + (member.symbol -> FunDef(member.params, member.body))
            }
          case _: TopLevel.TypeClassDecl =>
            ()
        }
    Env(List(Map.empty), functions, ctors, instances)

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

  // Compares two values, normalizing standard library constructors.
  private def valueEquals(left: Value, right: Value): Boolean =
    (valueToInt(left), valueToInt(right)) match
      case (Some(a), Some(b)) => a == b
      case _ =>
        (valueToBool(left), valueToBool(right)) match
          case (Some(a), Some(b)) => a == b
          case _ =>
            (valueToString(left), valueToString(right)) match
              case (Some(a), Some(b)) => a == b
              case _ =>
                (valueToList(left), valueToList(right)) match
                  case (Some(a), Some(b)) => a == b
                  case _ => left == right

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

  private def matchPattern(pattern: Pattern, value: Value, env: Env): Option[Map[TermSymbol, Value]] =
    val res = pattern match
      case Pattern.Wildcard() =>
        Some(Map.empty)
      case Pattern.Lit(literal) =>
        if valueEquals(literalValue(literal), value) then Some(Map.empty) else None
      case Pattern.Binder(symbol) =>
        Some(Map(symbol -> value))
      case Pattern.Ctor(symbol, args) =>
        value match
          case Value.AdtVal(ctorName, ctorArgs) if ctorName == symbol.name && ctorArgs.length == args.length =>
            matchPatternList(args, ctorArgs, env)
          case _ => None
    res

  private def matchPatternList(
      patterns: List[Pattern],
      values: List[Value],
      env: Env
    ): Option[Map[TermSymbol, Value]] =
    val results = patterns.zip(values).map { case (pat, v) => matchPattern(pat, v, env) }
    if results.forall(_.isDefined) then
      Some(results.flatten.foldLeft(Map.empty[TermSymbol, Value])(_ ++ _))
    else
      None

  private def iterableElements(value: Value): List[Value] =
    valueToList(value)
      .map(_.toList)
      .orElse {
        value match
          case Value.SetVal(values) => Some(values.toList)
          case Value.MapVal(values) => Some(values.keys.toList)
          case _ => None
      }
      .getOrElse(throw new IllegalArgumentException(s"Expected iterable, got: $value"))

  private def builtins: Map[String, Builtin] =
    Map(
      "println" -> builtinPrintln,
      "nativePrintln" -> builtinPrintln,
      "nativeIntPlus" -> builtinPlus,
      "nativeIntMinus" -> builtinMinus,
      "nativeIntTimes" -> builtinMul,
      "nativeIntDiv" -> builtinDiv,
      "nativeIntMod" -> builtinMod,
      "nativeIntNeg" -> builtinNeg,
      "nativeIntLt" -> builtinCompare(_ < 0),
      "nativeIntLe" -> builtinCompare(_ <= 0),
      "nativeBoolAnd" -> builtinBool(_ && _),
      "nativeBoolOr" -> builtinBool(_ || _),
      "nativeBoolNot" -> builtinNot,
      "nativeStringPlus" -> builtinStringPlus,
      "nativeStringLt" -> builtinStringCompare(_ < 0),
      "nativeStringLe" -> builtinStringCompare(_ <= 0),
      "nativeEq" -> builtinEq,
      "nativeShow" -> builtinShow,
      "nativeListConcat" -> builtinConcat,
      "nativeListGet" -> builtinGet,
      "nativeListSet" -> builtinSetIndex,
      "nativeListContains" -> builtinContains,
      "nativeSetContains" -> builtinContains,
      "nativeSetAdd" -> builtinAdd,
      "nativeMapPut" -> builtinPut,
      "nativeMapGet" -> builtinGet,
      "." -> builtinDot
    )

  private def builtinPrintln(args: List[Value], env: Env): (Value, Env) =
    // TODO do not use global state, pass in the PrintStream via the Env
    args match
      case List(value) =>
        val text = valueToString(value).getOrElse(renderValue(value))
        System.out.println(text)
        (Value.UnitVal, env)
      case other =>
        throw new IllegalArgumentException(s"nativePrintln expects 1 arg, got: $other")

  private def builtinPlus(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToInt(a), valueToInt(b)) match
          case (Some(left), Some(right)) => (intToValue(left + right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeIntPlus args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeIntPlus args: $other")

  private def builtinMinus(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToInt(a), valueToInt(b)) match
          case (Some(left), Some(right)) => (intToValue(left - right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeIntMinus args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeIntMinus args: $other")

  private def builtinMul(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToInt(a), valueToInt(b)) match
          case (Some(left), Some(right)) => (intToValue(left * right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeIntTimes args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeIntTimes args: $other")

  private def builtinDiv(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToInt(a), valueToInt(b)) match
          case (Some(left), Some(right)) => (intToValue(left / right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeIntDiv args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeIntDiv args: $other")

  private def builtinMod(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToInt(a), valueToInt(b)) match
          case (Some(left), Some(right)) => (intToValue(left % right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeIntMod args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeIntMod args: $other")

  // Negates an integer value.
  private def builtinNeg(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a) =>
        valueToInt(a) match
          case Some(value) => (intToValue(-value), env)
          case None => throw new IllegalArgumentException(s"Unsupported nativeIntNeg args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeIntNeg args: $other")

  private def builtinCompare(pred: Int => Boolean): Builtin =
    (args, env) =>
      args match
        case List(a, b) =>
          (valueToInt(a), valueToInt(b)) match
            case (Some(left), Some(right)) => (boolToValue(pred(left.compare(right))), env)
            case _ => throw new IllegalArgumentException(s"Unsupported nativeInt comparison args: $args")
        case other => throw new IllegalArgumentException(s"Unsupported nativeInt comparison args: $other")

  // Concatenates two strings.
  private def builtinStringPlus(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToString(a), valueToString(b)) match
          case (Some(left), Some(right)) => (stringToValue(left + right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeStringPlus args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeStringPlus args: $other")

  // Compares two strings using the supplied predicate.
  private def builtinStringCompare(pred: Int => Boolean): Builtin =
    (args, env) =>
      args match
        case List(a, b) =>
          (valueToString(a), valueToString(b)) match
            case (Some(left), Some(right)) => (boolToValue(pred(left.compareTo(right))), env)
            case _ => throw new IllegalArgumentException(s"Unsupported nativeString comparison args: $args")
        case other => throw new IllegalArgumentException(s"Unsupported nativeString comparison args: $other")

  private def builtinEq(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) => (boolToValue(valueEquals(a, b)), env)
      case other => throw new IllegalArgumentException(s"Unsupported equality args: $other")

  private def builtinBool(op: (Boolean, Boolean) => Boolean): Builtin =
    (args, env) =>
      args match
        case List(a, b) =>
          (valueToBool(a), valueToBool(b)) match
            case (Some(left), Some(right)) => (boolToValue(op(left, right)), env)
            case _ => throw new IllegalArgumentException(s"Unsupported boolean args: $args")
        case other => throw new IllegalArgumentException(s"Unsupported boolean args: $other")

  // Negates a boolean value.
  private def builtinNot(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a) =>
        valueToBool(a) match
          case Some(value) => (boolToValue(!value), env)
          case None => throw new IllegalArgumentException(s"Unsupported nativeBoolNot args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeBoolNot args: $other")

  private def builtinList(args: List[Value], env: Env): (Value, Env) =
    (listToValue(args.toVector), env)

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
      case List(value) =>
        valueToList(value) match
          case Some(values) => (intToValue(values.length), env)
          case None =>
            valueToString(value) match
              case Some(text) => (intToValue(text.length), env)
              case None =>
                value match
                  case Value.SetVal(values) => (intToValue(values.size), env)
                  case Value.MapVal(values) => (intToValue(values.size), env)
                  case other => throw new IllegalArgumentException(s"Unsupported len args: $other")

  private def builtinGet(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(collection, indexValue) =>
        valueToList(collection) match
          case Some(values) =>
            valueToInt(indexValue) match
              case Some(index) =>
                val i = index.toInt
                if i < 0 || i >= values.length then
                  throw new IllegalArgumentException(s"List index out of bounds: $i")
                (values(i), env)
              case None =>
                throw new IllegalArgumentException(s"List index is not an Int: $indexValue")
          case None =>
            collection match
              case Value.MapVal(values) =>
                values.get(indexValue) match
                  case Some(value) => (value, env)
                  case None => throw new IllegalArgumentException(s"Missing key: ${renderValue(indexValue)}")
              case other => throw new IllegalArgumentException(s"Unsupported get args: $other")
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
        (boolToValue(values.exists(valueEquals(_, value))), env)
      case List(collection, value) =>
        valueToList(collection) match
          case Some(values) => (boolToValue(values.exists(valueEquals(_, value))), env)
          case None =>
            collection match
              case Value.MapVal(values) => (boolToValue(values.contains(value)), env)
              case other => throw new IllegalArgumentException(s"Unsupported contains args: $other")
      case other => throw new IllegalArgumentException(s"Unsupported contains args: $other")

  private def builtinKeys(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values)) => (listToValue(values.keys.toVector), env)
      case other => throw new IllegalArgumentException(s"Unsupported keys args: $other")

  private def builtinValues(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values)) => (listToValue(values.values.toVector), env)
      case other => throw new IllegalArgumentException(s"Unsupported values args: $other")

  private def builtinAppend(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(collection, value) =>
        valueToList(collection) match
          case Some(values) => (listToValue(values :+ value), env)
          case None => throw new IllegalArgumentException(s"Unsupported append args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported append args: $other")

  private def builtinConcat(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(a, b) =>
        (valueToList(a), valueToList(b)) match
          case (Some(left), Some(right)) => (listToValue(left ++ right), env)
          case _ => throw new IllegalArgumentException(s"Unsupported concat args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported concat args: $other")

  private def builtinSlice(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(collection, startValue, endValue) =>
        (valueToList(collection), valueToInt(startValue), valueToInt(endValue)) match
          case (Some(values), Some(start), Some(end)) =>
            val s = start.toInt
            val e = end.toInt
            (listToValue(values.slice(s, e)), env)
          case _ => throw new IllegalArgumentException(s"Unsupported slice args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported slice args: $other")

  // Updates a list element at the given index.
  private def builtinSetIndex(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(collection, indexValue, value) =>
        (valueToList(collection), valueToInt(indexValue)) match
          case (Some(values), Some(index)) =>
            val i = index.toInt
            if i < 0 || i >= values.length then
              throw new IllegalArgumentException(s"List index out of bounds: $i")
            val updated = values.updated(i, value)
            (listToValue(updated), env)
          case _ => throw new IllegalArgumentException(s"Unsupported nativeListSet args: $args")
      case other => throw new IllegalArgumentException(s"Unsupported nativeListSet args: $other")

  // Renders a value into a String constructor value.
  private def builtinShow(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(value) => (stringToValue(renderValue(value)), env)
      case other => throw new IllegalArgumentException(s"Unsupported nativeShow args: $other")

  private def builtinDot(args: List[Value], env: Env): (Value, Env) =
    args match
      case List(Value.MapVal(values), fieldValue) =>
        val key = valueToString(fieldValue).map(Value.StringVal.apply).getOrElse(fieldValue)
        values.get(key) match
          case Some(value) => (value, env)
          case None => throw new IllegalArgumentException(s"Missing map field: ${renderValue(fieldValue)}")
      case other => throw new IllegalArgumentException(s"Unsupported . args: $other")

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
          case Value.InstanceVal(_) => "<instance>"
          case Value.AdtVal(name, args) =>
            if args.isEmpty then name else s"$name${args.map(renderValue).mkString("(", ", ", ")")}"
          case Value.UnitVal => "unit"
          case Value.UndefinedVal => "undefined"
          case Value.FuncVal(_) => "<function>"
          case Value.IntVal(v) => v.toString
          case Value.BoolVal(v) => v.toString
          case Value.StringVal(v) => v
          case Value.ListVal(values) => values.map(renderValue).mkString("[", ", ", "]")
      }

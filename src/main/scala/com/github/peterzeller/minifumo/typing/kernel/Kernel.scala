package com.github.peterzeller.minifumo.typing.kernel

object Kernel {
  enum Reducibility {
    case Reducible
    case Opaque
  }

  sealed trait Term

  object Term {
    final case class Var(index: Int) extends Term
    final case class Sort(level: Int) extends Term
    final case class Pi(dom: Term, cod: Term) extends Term
    final case class Lam(dom: Term, body: Term) extends Term
    final case class App(fn: Term, arg: Term) extends Term
    final case class Let(value: Term, valueTy: Term, body: Term) extends Term
    final case class Const(name: String) extends Term
  }

  final case class InductiveConstructor(name: String, typ: Term)

  final case class InductiveType(
      name: String,
      typ: Term,
      constructors: List[InductiveConstructor],
      paramCount: Int
  )

  final case class ConstDecl(typ: Term, value: Option[Term], reducibility: Reducibility)

  final case class Env(constants: Map[String, ConstDecl]) {
    /** Looks up a constant declaration by name. */
    def lookup(name: String): Option[ConstDecl] = constants.get(name)

    /** Returns a new environment with the given constant added. */
    def addConstant(name: String, decl: ConstDecl): Env = copy(constants = constants + (name -> decl))
  }

  object Env {
    /** Creates an empty environment. */
    def empty: Env = Env(Map.empty)
  }

  final case class Context(types: List[Term]) {
    /** Looks up a bound variable type by de Bruijn index. */
    def lookup(index: Int): Option[Term] = types.lift(index)

    /** Extends the context with a new bound variable type. */
    def extend(typ: Term): Context = Context(typ :: types)
  }

  object Context {
    /** Creates an empty local context. */
    def empty: Context = Context(Nil)
  }

  final case class KernelError(message: String) extends RuntimeException(message)

  enum Polarity {
    case Positive
    case Negative

    /** Flips polarity for negative positions. */
    def flip: Polarity = this match {
      case Positive => Negative
      case Negative => Positive
    }
  }

  enum CaseBinderKind {
    case Arg
    case InductionHypothesis
  }

  /** Shifts de Bruijn indices in a term by the given amount above a cutoff. */
  def lift(term: Term, by: Int, cutoff: Int): Term = term match {
    case Term.Var(index) if index >= cutoff => Term.Var(index + by)
    case Term.Var(index) => Term.Var(index)
    case Term.Sort(level) => Term.Sort(level)
    case Term.Pi(dom, cod) => Term.Pi(lift(dom, by, cutoff), lift(cod, by, cutoff + 1))
    case Term.Lam(dom, body) => Term.Lam(lift(dom, by, cutoff), lift(body, by, cutoff + 1))
    case Term.App(fn, arg) => Term.App(lift(fn, by, cutoff), lift(arg, by, cutoff))
    case Term.Let(value, valueTy, body) =>
      Term.Let(lift(value, by, cutoff), lift(valueTy, by, cutoff), lift(body, by, cutoff + 1))
    case Term.Const(name) => Term.Const(name)
  }

  /** Substitutes a term for a de Bruijn index at the given cutoff depth. */
  def subst(term: Term, index: Int, replacement: Term, cutoff: Int): Term = term match {
    case Term.Var(ix) if ix == index + cutoff => lift(replacement, cutoff, 0)
    case Term.Var(ix) => Term.Var(ix)
    case Term.Sort(level) => Term.Sort(level)
    case Term.Pi(dom, cod) =>
      Term.Pi(subst(dom, index, replacement, cutoff), subst(cod, index, replacement, cutoff + 1))
    case Term.Lam(dom, body) =>
      Term.Lam(subst(dom, index, replacement, cutoff), subst(body, index, replacement, cutoff + 1))
    case Term.App(fn, arg) =>
      Term.App(subst(fn, index, replacement, cutoff), subst(arg, index, replacement, cutoff))
    case Term.Let(value, valueTy, body) =>
      Term.Let(
        subst(value, index, replacement, cutoff),
        subst(valueTy, index, replacement, cutoff),
        subst(body, index, replacement, cutoff + 1)
      )
    case Term.Const(name) => Term.Const(name)
  }

  /** Substitutes a term for a de Bruijn index at the top level. */
  def subst(term: Term, index: Int, replacement: Term): Term = subst(term, index, replacement, 0)

  /** Beta-reduces by substituting an argument into a lambda body. */
  def betaReduce(body: Term, arg: Term): Term = lift(subst(body, 0, lift(arg, 1, 0)), -1, 0)

  /** Computes weak head normal form for a term in the given environment and context. */
  def whnf(env: Env, ctx: Context, term: Term): Term = term match {
    case Term.App(fn, arg) =>
      whnf(env, ctx, fn) match {
        case Term.Lam(_, body) => whnf(env, ctx, betaReduce(body, arg))
        case reducedFn => Term.App(reducedFn, arg)
      }
    case Term.Let(value, _, body) => whnf(env, ctx, betaReduce(body, value))
    case Term.Const(name) =>
      env.lookup(name) match {
        case Some(ConstDecl(_, Some(value), Reducibility.Reducible)) => whnf(env, ctx, value)
        case _ => term
      }
    case _ => term
  }

  /** Checks definitional equality by reducing to WHNF and structurally comparing. */
  def conv(env: Env, ctx: Context, t1: Term, t2: Term): Boolean =
    (whnf(env, ctx, t1), whnf(env, ctx, t2)) match {
      case (Term.Var(ix1), Term.Var(ix2)) => ix1 == ix2
      case (Term.Sort(u1), Term.Sort(u2)) => u1 == u2
      case (Term.Const(n1), Term.Const(n2)) => n1 == n2
      case (Term.App(fn1, arg1), Term.App(fn2, arg2)) =>
        conv(env, ctx, fn1, fn2) && conv(env, ctx, arg1, arg2)
      case (Term.Pi(dom1, cod1), Term.Pi(dom2, cod2)) =>
        conv(env, ctx, dom1, dom2) && conv(env, ctx.extend(dom1), cod1, cod2)
      case (Term.Lam(dom1, body1), Term.Lam(dom2, body2)) =>
        conv(env, ctx, dom1, dom2) && conv(env, ctx.extend(dom1), body1, body2)
      case _ => false
    }

  /** Infers the type of a term or throws a KernelError when inference fails. */
  def infer(env: Env, ctx: Context, term: Term): Term = term match {
    case Term.Var(index) =>
      ctx.lookup(index).getOrElse(throw KernelError(s"Unbound variable index $index"))
    case Term.Sort(level) => Term.Sort(level + 1)
    case Term.Const(name) =>
      env.lookup(name).map(_.typ).getOrElse(throw KernelError(s"Unknown constant $name"))
    case Term.Pi(dom, cod) =>
      val domSort = infer(env, ctx, dom)
      val domLevel = ensureSort(env, ctx, domSort)
      val codSort = infer(env, ctx.extend(dom), cod)
      val codLevel = ensureSort(env, ctx.extend(dom), codSort)
      Term.Sort(math.max(domLevel, codLevel))
    case Term.Lam(dom, body) =>
      val domSort = infer(env, ctx, dom)
      ensureSort(env, ctx, domSort)
      val bodyTy = infer(env, ctx.extend(dom), body)
      Term.Pi(dom, bodyTy)
    case Term.App(fn, arg) =>
      whnf(env, ctx, infer(env, ctx, fn)) match {
        case Term.Pi(dom, cod) =>
          check(env, ctx, arg, dom)
          betaReduce(cod, arg)
        case other => throw KernelError(s"Expected function type, found $other")
      }
    case Term.Let(value, valueTy, body) =>
      val valueTySort = infer(env, ctx, valueTy)
      ensureSort(env, ctx, valueTySort)
      check(env, ctx, value, valueTy)
      val bodyTy = infer(env, ctx.extend(valueTy), body)
      betaReduce(bodyTy, value)
  }

  /** Checks that a term has the expected type up to definitional equality. */
  def check(env: Env, ctx: Context, term: Term, expected: Term): Unit = {
    val inferred = infer(env, ctx, term)
    if !conv(env, ctx, inferred, expected) then
      throw KernelError(s"Type mismatch: inferred $inferred but expected $expected")
  }

  /** Ensures that a term is a sort and returns its universe level. */
  private def ensureSort(env: Env, ctx: Context, term: Term): Int = whnf(env, ctx, term) match {
    case Term.Sort(level) => level
    case other => throw KernelError(s"Expected a sort, found $other")
  }

  /** Collects nested Pi binders and returns their domains plus the final body. */
  private def collectPis(term: Term): (List[Term], Term) = term match {
    case Term.Pi(dom, cod) =>
      val (rest, body) = collectPis(cod)
      (dom :: rest, body)
    case other => (Nil, other)
  }

  /** Builds a Pi chain from a list of binder types and a body. */
  private def mkPi(binders: List[Term], body: Term): Term =
    binders.foldRight(body)(Term.Pi.apply)

  /** Collects applications into a head and argument list. */
  private def collectApps(term: Term): (Term, List[Term]) = term match {
    case Term.App(fn, arg) =>
      val (head, args) = collectApps(fn)
      (head, args :+ arg)
    case other => (other, Nil)
  }

  /** Builds a nested application from a head term and arguments. */
  private def mkApp(head: Term, args: List[Term]): Term =
    args.foldLeft(head)(Term.App.apply)

  /** Ensures that an inductive occurrence is strictly positive in a type. */
  private def ensurePositive(inductiveName: String, term: Term): Unit = {
    /** Checks whether the inductive constant appears anywhere in a term. */
    def containsInductive(target: Term): Boolean = target match {
      case Term.Const(name) => name == inductiveName
      case Term.Var(_) => false
      case Term.Sort(_) => false
      case Term.Pi(dom, cod) => containsInductive(dom) || containsInductive(cod)
      case Term.Lam(dom, body) => containsInductive(dom) || containsInductive(body)
      case Term.App(fn, arg) => containsInductive(fn) || containsInductive(arg)
      case Term.Let(value, valueTy, body) =>
        containsInductive(value) || containsInductive(valueTy) || containsInductive(body)
    }

    /** Recursively checks polarity of inductive occurrences. */
    def check(term: Term, polarity: Polarity): Unit = term match {
      case _ if polarity == Polarity.Negative && containsInductive(term) =>
        throw KernelError(s"Inductive $inductiveName occurs in a negative position.")
      case Term.Pi(dom, cod) =>
        check(dom, polarity.flip)
        check(cod, polarity)
      case Term.Lam(dom, body) =>
        check(dom, polarity.flip)
        check(body, polarity)
      case Term.App(_, _) if polarity == Polarity.Positive =>
        val (head, args) = collectApps(term)
        head match {
          case Term.Const(name) if name == inductiveName =>
            if args.exists(containsInductive) then
              throw KernelError(s"Inductive $inductiveName appears in constructor arguments.")
          case _ =>
            if containsInductive(term) then
              throw KernelError(s"Inductive $inductiveName occurs under a non-positive type constructor.")
        }
      case Term.App(_, _) => ()
      case Term.Let(value, valueTy, body) =>
        check(value, polarity)
        check(valueTy, polarity)
        check(body, polarity)
      case _ => ()
    }

    check(term, Polarity.Positive)
  }

  /** Adds an inductive type with constructors and a recursor after checks. */
  def addInductive(env: Env, inductive: InductiveType): Env = {
    val names = inductive.name :: inductive.constructors.map(_.name) ::: List(s"${inductive.name}.rec")
    names.foreach { name =>
      if env.lookup(name).isDefined then
        throw KernelError(s"Constant $name already declared.")
    }

    val (binders, body) = collectPis(inductive.typ)
    if inductive.paramCount > binders.length then
      throw KernelError(s"Inductive ${inductive.name} has too many parameters.")
    val ctxForInductive = binders.foldLeft(Context.empty)(_.extend(_))
    val resultLevel = ensureSort(env, ctxForInductive, body)
    ensureSort(env, Context.empty, infer(env, Context.empty, inductive.typ))

    val params = binders.take(inductive.paramCount)
    val indices = binders.drop(inductive.paramCount)

    val envWithInductive =
      env.addConstant(inductive.name, ConstDecl(inductive.typ, None, Reducibility.Opaque))

    inductive.constructors.foreach { ctor =>
      val (ctorBinders, ctorResult) = collectPis(ctor.typ)
      if ctorBinders.length < inductive.paramCount then
        throw KernelError(s"Constructor ${ctor.name} has too few parameters.")
      var ctx = Context.empty
      params.zip(ctorBinders.take(inductive.paramCount)).foreach { (expected, actual) =>
        if !conv(envWithInductive, ctx, actual, expected) then
          throw KernelError(s"Constructor ${ctor.name} parameter type mismatch.")
        ctx = ctx.extend(actual)
      }
      val ctorArgs = ctorBinders.drop(inductive.paramCount)
      ctorArgs.foreach { argType =>
        ensurePositive(inductive.name, argType)
      }
      val (resultHead, resultArgs) = collectApps(ctorResult)
      resultHead match {
        case Term.Const(name) if name == inductive.name =>
          val totalArgs = params.length + indices.length
          if resultArgs.length != totalArgs then
            throw KernelError(s"Constructor ${ctor.name} must return ${inductive.name} applied to $totalArgs arguments.")
          val totalBinders = params.length + ctorArgs.length
          params.indices.foreach { paramIndex =>
            val expectedIndex = totalBinders - 1 - paramIndex
            resultArgs(paramIndex) match {
              case Term.Var(ix) if ix == expectedIndex => ()
              case other =>
                throw KernelError(s"Constructor ${ctor.name} must return ${inductive.name} applied to parameters, found $other.")
            }
          }
        case _ =>
          throw KernelError(s"Constructor ${ctor.name} must return ${inductive.name}.")
      }
      val ctorLevel = ensureSort(envWithInductive, Context.empty, infer(envWithInductive, Context.empty, ctor.typ))
      if ctorLevel > resultLevel + 1 then
        throw KernelError(s"Constructor ${ctor.name} lives in a higher universe than ${inductive.name}.")
    }

    val caseTypes = inductive.constructors.foldLeft(List.empty[Term]) { (acc, ctor) =>
      val baseContextSize = params.length + indices.length + 1 + acc.length
      val (ctorBinders, _) = collectPis(ctor.typ)
      val ctorArgs = ctorBinders.drop(inductive.paramCount)
      val extraBinders = indices.length + 1 + acc.length

      var ihCount = 0
      val caseBinders = ctorArgs.zipWithIndex.foldLeft(List.empty[(CaseBinderKind, Term, Int)]) {
        case (binders, (argType, argIndex)) =>
          val liftBy = extraBinders + ihCount
          val liftedArgType = lift(argType, liftBy, 0)
          val contextBeforeArg = baseContextSize + binders.length
          val (head, args) = collectApps(liftedArgType)
          val totalArgs = params.length + indices.length
          val paramVars = params.indices.map { paramIndex =>
            Term.Var(contextBeforeArg - 1 - paramIndex)
          }.toList
          val isRecursive =
            head match {
              case Term.Const(name) if name == inductive.name && args.length == totalArgs =>
                args.take(params.length) == paramVars
              case _ => false
            }
          val withArg = binders :+ (CaseBinderKind.Arg, liftedArgType, argIndex)
          if isRecursive then
            val contextWithArg = contextBeforeArg + 1
            val motiveIndex = contextWithArg - 1 - (params.length + indices.length)
            val ihType = Term.App(Term.Var(motiveIndex), Term.Var(0))
            ihCount += 1
            withArg :+ (CaseBinderKind.InductionHypothesis, ihType, argIndex)
          else
            withArg
      }

      val totalContextSize = baseContextSize + caseBinders.length
      val paramVars = params.indices.map { paramIndex =>
        Term.Var(totalContextSize - 1 - paramIndex)
      }.toList
      val argVars = ctorArgs.indices.map { argIndex =>
        val binderPos = caseBinders.indexWhere { case (kind, _, idx) =>
          kind == CaseBinderKind.Arg && idx == argIndex
        }
        if binderPos == -1 then
          throw KernelError(s"Constructor ${ctor.name} argument bookkeeping failed.")
        Term.Var(totalContextSize - 1 - binderPos)
      }.toList
      val ctorApp = mkApp(Term.Const(ctor.name), paramVars ++ argVars)
      val motiveIndex = totalContextSize - 1 - (params.length + indices.length)
      val body = Term.App(Term.Var(motiveIndex), ctorApp)
      acc :+ mkPi(caseBinders.map(_._2), body)
    }

    val motiveContextSize = params.length + indices.length
    val motiveParamVars = params.indices.map { paramIndex =>
      Term.Var(motiveContextSize - 1 - paramIndex)
    }
    val motiveIndexVars = indices.indices.map { indexIndex =>
      Term.Var(motiveContextSize - 1 - (params.length + indexIndex))
    }
    val motiveTarget = mkApp(Term.Const(inductive.name), (motiveParamVars ++ motiveIndexVars).toList)
    val motiveType = Term.Pi(motiveTarget, Term.Sort(resultLevel))

    val targetContextSize = params.length + indices.length + 1 + caseTypes.length
    val targetParamVars = params.indices.map { paramIndex =>
      Term.Var(targetContextSize - 1 - paramIndex)
    }
    val targetIndexVars = indices.indices.map { indexIndex =>
      Term.Var(targetContextSize - 1 - (params.length + indexIndex))
    }
    val targetType = mkApp(Term.Const(inductive.name), (targetParamVars ++ targetIndexVars).toList)

    val totalContextSize =
      params.length + indices.length + 1 + caseTypes.length + 1
    val motiveIndex = totalContextSize - 1 - (params.length + indices.length)
    val recBody = Term.App(Term.Var(motiveIndex), Term.Var(0))
    val recType = mkPi(params ++ indices ++ (motiveType :: caseTypes) :+ targetType, recBody)

    val envWithConstructors = inductive.constructors.foldLeft(envWithInductive) { (acc, ctor) =>
      acc.addConstant(ctor.name, ConstDecl(ctor.typ, None, Reducibility.Opaque))
    }
    envWithConstructors.addConstant(s"${inductive.name}.rec", ConstDecl(recType, None, Reducibility.Opaque))
  }
}

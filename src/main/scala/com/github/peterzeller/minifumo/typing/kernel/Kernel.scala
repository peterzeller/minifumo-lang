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
    def extend(typ: Term): Context = {
      val liftedExisting = types.map(existing => lift(existing, 1, 0))
      Context(lift(typ, 1, 0) :: liftedExisting)
    }
  }

  object Context {
    /** Creates an empty local context. */
    def empty: Context = Context(Nil)
  }

  final case class KernelError(message: String) extends RuntimeException(message)

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

  /** Substitutes a term for a de Bruijn index, shifting under binders as needed. */
  def subst(term: Term, index: Int, replacement: Term): Term = term match {
    case Term.Var(ix) if ix == index => replacement
    case Term.Var(ix) => Term.Var(ix)
    case Term.Sort(level) => Term.Sort(level)
    case Term.Pi(dom, cod) => Term.Pi(subst(dom, index, replacement), subst(cod, index + 1, lift(replacement, 1, 0)))
    case Term.Lam(dom, body) => Term.Lam(subst(dom, index, replacement), subst(body, index + 1, lift(replacement, 1, 0)))
    case Term.App(fn, arg) => Term.App(subst(fn, index, replacement), subst(arg, index, replacement))
    case Term.Let(value, valueTy, body) =>
      Term.Let(
        subst(value, index, replacement),
        subst(valueTy, index, replacement),
        subst(body, index + 1, lift(replacement, 1, 0))
      )
    case Term.Const(name) => Term.Const(name)
  }

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
      val domLevel = ensureSort(domSort)
      val codSort = infer(env, ctx.extend(dom), cod)
      val codLevel = ensureSort(codSort)
      Term.Sort(math.max(domLevel, codLevel))
    case Term.Lam(dom, body) =>
      val domSort = infer(env, ctx, dom)
      ensureSort(domSort)
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
      ensureSort(valueTySort)
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
  private def ensureSort(term: Term): Int = term match {
    case Term.Sort(level) => level
    case other => throw KernelError(s"Expected a sort, found $other")
  }
}

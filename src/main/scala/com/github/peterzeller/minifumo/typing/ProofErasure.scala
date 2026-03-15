package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.typing.TypedAst.{CtorDecl, Expr, FunSig, MatchCase, Pattern, Program, TopLevel}

/** Erases proof- and type-level artifacts from a typed program before evaluation. */
object ProofErasure:

  /** Returns an erased copy of the program suitable for runtime interpretation. */
  def erase(program: Program): Program =
    Program(program.items.map(eraseTopLevel))(program.source)

  /** Erases one top-level declaration. */
  private def eraseTopLevel(topLevel: TopLevel): TopLevel =
    topLevel match
      case dataDecl@TopLevel.DataDecl(symbol, _, ctors) =>
        TopLevel.DataDecl(symbol, List(), ctors.map(eraseCtor))(dataDecl.comment)(topLevel.source)
      case funDecl@TopLevel.FunDecl(sig, body) =>
        val keptParams = sig.params.filterNot(param => isErasedParameterType(param.tpe))
        val erasedSig = FunSig(sig.symbol, List(), keptParams, eraseExpr(sig.returnType))
        TopLevel.FunDecl(erasedSig, eraseExpr(body))(funDecl.comment)(topLevel.source)


  /** Erases all constructor fields so constructors carry no runtime payload. */
  private def eraseCtor(ctor: CtorDecl): CtorDecl =
    CtorDecl(ctor.symbol, List(), List(), eraseExpr(ctor.returnType), eraseExpr(ctor.tpe))(ctor.comment)(ctor.source)

  /** Erases proof and type terms inside one expression tree. */
  private def eraseExpr(expr: Expr): Expr =
    if isErasedValue(expr) then
      unitExpr(expr.source)
    else
      expr match
        case Expr.Lit(_) | Expr.Var(_) | Expr.Sort(_) | Expr.Meta(_, _) | Expr.UnknownType() | Expr.Axiom() =>
          expr
        case Expr.App(callee, arg, tpe) =>
          if isErasedValue(arg) then eraseExpr(callee)
          else expr
        case Expr.AppImplicit(callee, arg, tpe) =>
          if isErasedValue(arg) then eraseExpr(callee)
          else expr
        case Expr.Pi(dom, cod, isImplicit) =>
          Expr.Pi(dom, eraseExpr(cod), isImplicit)(expr.source)
        case Expr.Lambda(param, body, tpe) =>
          Expr.Lambda(param, eraseExpr(body), eraseExpr(tpe))(expr.source)
        case letExpr@Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
          Expr.LetIn(symbol, isConstant, eraseExpr(declaredType), eraseExpr(value), eraseExpr(body))(letExpr.comment)(expr.source)
        case Expr.Match(scrutinee, motive, cases) =>
          if isErasedValue(scrutinee) then
            eraseExpr(cases.headOption.map(_.body).getOrElse(unitExpr(expr.source)))
          else
            Expr.Match(eraseExpr(scrutinee), eraseExpr(motive), cases.map(eraseCase))(expr.source)

  /** Erases one match case and its pattern structure. */
  private def eraseCase(matchCase: MatchCase): MatchCase =
    MatchCase(erasePattern(matchCase.pattern), eraseExpr(matchCase.body))(matchCase.source)

  /** Erases constructor pattern payloads for proof-only bindings. */
  private def erasePattern(pattern: Pattern): Pattern =
    pattern match
      case Pattern.Ctor(symbol, args) =>
        Pattern.Ctor(symbol, args.filterNot(isErasedPatternArg).map(erasePattern))(pattern.source)
      case _ =>
        pattern

  /** Returns true when this pattern argument binds an erased proof/type value. */
  private def isErasedPatternArg(pattern: Pattern): Boolean =
    pattern match
      case Pattern.Binder(symbol) => isErasedParameterType(symbol.tpe)
      case _ => false

  /** Creates a unit literal expression at the given source range. */
  private def unitExpr(source: SourceRange): Expr =
    Expr.Lit(Literal.UnitLit()(source))(source)

  /** Returns true when a parameter type is erasable (proof-level or type-level). */
  private def isErasedParameterType(tpe: Expr): Boolean =
    isSortType(tpe) || isPropositionType(tpe)

  /** Returns true when a term value can be erased to unit. */
  private def isErasedValue(expr: Expr): Boolean =
    isSortTypeValue(expr) || isPropositionValue(expr)

  /** Returns true when the expression itself is a type-level term (has sort as direct type). */
  private def isSortTypeValue(expr: Expr): Boolean =
    expr.calculateType match
      case Some(Expr.Sort(_)) => true
      case _ => false

  /** Returns true when the expression has a proposition type. */
  private def isPropositionValue(expr: Expr): Boolean =
    expr.calculateType.exists(isPropositionType)

  /** Returns true when an expression is itself a sort-level type. */
  private def isSortType(expr: Expr): Boolean =
    expr match
      case Expr.Sort(_) => true
      case _ => false

  /** Returns true when the given type expression inhabits Prop. */
  private def isPropositionType(tpe: Expr): Boolean =
    tpe.calculateType match
      case Some(Expr.Sort(UniverseLevel.Prop)) => true
      case _ => false

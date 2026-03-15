package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.typing.TypedAst.{CtorDecl, Expr, FunSig, MatchCase, Pattern, Program, TopLevel}

/** Provides reusable pre-order rewriting utilities for typed AST trees. */
object TypedAstRewriter:

  /** Rewrites a typed AST node in pre-order using the supplied partial node rewrite rule. */
  def rewrite[T <: TypedAst](node: T)(rule: PartialFunction[TypedAst, TypedAst]): T =
    val rewrittenNode = applyRule(node, rule)
    rewriteChildren(rewrittenNode, rule).asInstanceOf[T]

  /** Applies the rewrite rule to a node when it is defined for that node. */
  private def applyRule(node: TypedAst, rule: PartialFunction[TypedAst, TypedAst]): TypedAst =
    rule.applyOrElse(node, identity[TypedAst])

  /** Rewrites all direct children of one node after a possible pre-order rewrite. */
  private def rewriteChildren(node: TypedAst, rule: PartialFunction[TypedAst, TypedAst]): TypedAst =
    node match
      case program: Program =>
        Program(program.items.map(item => rewrite(item)(rule)))(program.source)
      case dataDecl@TopLevel.DataDecl(symbol, typeParams, ctors) =>
        val rewrittenTypeParams = typeParams.map(param => rewriteLocalSymbol(param, rule))
        val rewrittenCtors = ctors.map(ctor => rewrite(ctor)(rule))
        TopLevel.DataDecl(symbol, rewrittenTypeParams, rewrittenCtors)(dataDecl.comment)(node.source)
      case funDecl@TopLevel.FunDecl(sig, body) =>
        TopLevel.FunDecl(rewrite(sig)(rule), rewrite(body)(rule))(funDecl.comment)(node.source)
      case funSig: FunSig =>
        val rewrittenTypeParams = funSig.typeParams.map(param => rewriteLocalSymbol(param, rule))
        val rewrittenParams = funSig.params.map(param => rewriteLocalSymbol(param, rule))
        val rewrittenReturnType = rewrite(funSig.returnType)(rule)
        FunSig(funSig.symbol, rewrittenTypeParams, rewrittenParams, rewrittenReturnType)
      case ctorDecl: CtorDecl =>
        val rewrittenImplicitFields = ctorDecl.implicitFields.map(field => rewriteLocalSymbol(field, rule))
        val rewrittenFields = ctorDecl.fields.map(field => rewriteLocalSymbol(field, rule))
        val rewrittenReturnType = rewrite(ctorDecl.returnType)(rule)
        val rewrittenType = rewrite(ctorDecl.tpe)(rule)
        CtorDecl(ctorDecl.symbol, rewrittenImplicitFields, rewrittenFields, rewrittenReturnType, rewrittenType)(ctorDecl.comment)(node.source)
      case Expr.App(callee, arg, tpe) =>
        Expr.App(rewrite(callee)(rule), rewrite(arg)(rule), rewrite(tpe)(rule))(node.source)
      case Expr.AppImplicit(callee, arg, tpe) =>
        Expr.AppImplicit(rewrite(callee)(rule), rewrite(arg)(rule), rewrite(tpe)(rule))(node.source)
      case Expr.Pi(dom, cod, isImplicit) =>
        Expr.Pi(rewriteLocalSymbol(dom, rule), rewrite(cod)(rule), isImplicit)(node.source)
      case Expr.Lambda(param, body, tpe) =>
        Expr.Lambda(rewriteLocalSymbol(param, rule), rewrite(body)(rule), rewrite(tpe)(rule))(node.source)
      case letExpr@Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        Expr.LetIn(
          rewriteLocalSymbol(symbol, rule),
          isConstant,
          rewrite(declaredType)(rule),
          rewrite(value)(rule),
          rewrite(body)(rule)
        )(letExpr.comment)(node.source)
      case meta: Expr.Meta =>
        Expr.Meta(meta.index, rewrite(meta.tpe)(rule))(meta.name, node.source)
      case Expr.Match(scrutinee, motive, cases) =>
        val rewrittenCases = cases.map(matchCase => rewrite(matchCase)(rule))
        Expr.Match(rewrite(scrutinee)(rule), rewrite(motive)(rule), rewrittenCases)(node.source)
      case matchCase: MatchCase =>
        MatchCase(rewrite(matchCase.pattern)(rule), rewrite(matchCase.body)(rule))(node.source)
      case Pattern.Binder(symbol) =>
        Pattern.Binder(rewriteLocalSymbol(symbol, rule))(node.source)
      case Pattern.Ctor(symbol, args) =>
        Pattern.Ctor(symbol, args.map(arg => rewrite(arg)(rule)))(node.source)
      case other =>
        other

  /** Rewrites a local symbol by rewriting its type annotation while preserving identity metadata. */
  private def rewriteLocalSymbol(symbol: LocalSymbol, rule: PartialFunction[TypedAst, TypedAst]): LocalSymbol =
    LocalSymbol(symbol.name, rewrite(symbol.tpe)(rule), symbol.id)


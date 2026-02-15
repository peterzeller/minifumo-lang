package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Utilities for creating and solving metavariables in the type checker. */
object TypeCheckerMetas:
  /** Placeholder hook for future implicit argument search. */
  def searchImplicitArgument(expectedType: TypedAst.Expr, source: SourceRange)(implicit ctx: Context): Option[TypedAst.Expr] =
    None

  /** Creates a fresh metavariable expression. */
  def freshMeta(name: String, tpe: TypedAst.Expr, source: ast.SourceRange)(implicit ids: IdSupply): TypedAst.Expr =
    TypedAst.Expr.Meta(ids.freshMetaId(), tpe)(name, source)

  /** Solves a meta-variable if the occurs check passes. */
  def solveMeta(metaId: Int, term: TypedAst.Expr)(implicit metas: MetaContext): Boolean =
    if occurs(metaId, term) then
      false
    else
      metas.assign(metaId, term)
      true

  /** Performs an occurs check for metas in a typed term. */
  def occurs(metaId: Int, term: TypedAst.Expr): Boolean =
    term match
      case TypedAst.Expr.Meta(id, _) => id == metaId
      case TypedAst.Expr.App(callee, arg, _) => occurs(metaId, callee) || occurs(metaId, arg)
      case TypedAst.Expr.AppImplicit(callee, arg, _) => occurs(metaId, callee) || occurs(metaId, arg)
      case TypedAst.Expr.Lambda(_, body, _) => occurs(metaId, body)
      case TypedAst.Expr.LetIn(_, _, _, value, body) => occurs(metaId, value) || occurs(metaId, body)
      case TypedAst.Expr.Pi(dom, cod, _) => occurs(metaId, dom.tpe) || occurs(metaId, cod)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        occurs(metaId, scrutinee) || cases.exists(c => occurs(metaId, c.body))
      case _ => false

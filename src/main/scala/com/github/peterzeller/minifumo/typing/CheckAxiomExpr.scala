package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for axiom expressions. */
object CheckAxiomExpr:
  /** Infers an axiom expression as unknown, because axioms require an expected type. */
  def infer(expr: ast.Expr.Axiom)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val _ = (ctx, metas, ids)
    val typedExpr = TypedAst.Expr.Axiom()(expr.source)
    val inferredType = TypedAst.Expr.UnknownType()(expr.source)
    (typedExpr, inferredType, Nil)

  /** Checks an axiom expression against any expected type. */
  def check(expr: ast.Expr.Axiom, expectedType: TypedAst.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val _ = (expectedType, ctx, metas, ids)
    (TypedAst.Expr.Axiom()(expr.source), Nil)

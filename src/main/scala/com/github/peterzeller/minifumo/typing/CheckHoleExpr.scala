package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for hole expressions. */
object CheckHoleExpr:
  /** Infers a fresh meta-variable for a hole. */
  def infer(expr: ast.Expr.Hole)(using ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val meta = freshMeta("hole", TypedAst.Expr.UnknownType()(expr.source), expr.source)
    (meta, TypedAst.Expr.UnknownType()(expr.source), List())

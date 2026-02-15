package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast

/** Type-checking logic for hole expressions. */
object CheckHoleExpr:
  /** Infers a fresh meta-variable for a hole. */
  def infer(expr: ast.Expr.Hole, freshMeta: (String, TypedAst.Expr, ast.SourceRange) => TypedAst.Expr): (TypedAst.Expr, TypedAst.Expr, List[TypeChecker.TypeError]) =
    val meta = freshMeta("hole", TypedAst.Expr.UnknownType()(expr.source), expr.source)
    (meta, TypedAst.Expr.UnknownType()(expr.source), List())

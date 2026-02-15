package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for literal expressions. */
object CheckLiteralExpr:
  /** Infers the type for a literal expression. */
  def infer(expr: ast.Expr.Lit)(using ctx: TypeContext): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val typed = TypedAst.Expr.Lit(expr.value)(expr.source)
    val tpe = literalType(expr.value, ctx)
    (typed, tpe, List())

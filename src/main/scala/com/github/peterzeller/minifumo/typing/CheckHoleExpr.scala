package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for hole expressions. */
object CheckHoleExpr:
  /** Infers a fresh meta-variable for a hole. */
  def infer(expr: ast.Expr.Hole)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val meta = freshMeta("holeType", TypedAst.Expr.Sort(UniverseLevel.Type1)(expr.source), expr.source)
    metas.addConstraint(HoleConstraint(meta, expr.source))
    val sym = LocalSymbol("hole", meta, ids.freshLocalId())("")
    (TypedAst.Expr.Var(sym)(expr.source), meta, List())

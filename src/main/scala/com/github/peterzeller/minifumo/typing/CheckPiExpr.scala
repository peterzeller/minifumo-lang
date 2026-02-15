package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for pi type expressions. */
object CheckPiExpr:
  /** Infers the sort for a pi type expression. */
  def infer(
      expr: ast.Expr.Pi,
      signatureExpr: (ast.Expr, GlobalEnv, Map[String, TypedAst.TermSymbol]) => TypedAst.Expr
    )(using ids: IdSupply, ctx: TypeContext): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val dom = signatureExpr(expr.param.tpe, ctx.globals, Map())
    val cod = signatureExpr(expr.body, ctx.globals, Map())
    val sym = TypedAst.LocalSymbol(expr.param.name, dom, ids.freshLocalId())
    val piExpr = TypedAst.Expr.Pi(sym, cod, isImplicit = false)(expr.source)
    (piExpr, TypedAst.Expr.Sort()(expr.source), List())

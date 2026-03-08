package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for pi type expressions. */
object CheckPiExpr:
  /** Infers the sort for a pi type expression. */
  def infer(expr: ast.Expr.Pi)(using ids: IdSupply, ctx: TypeContext, metas: MetaContext): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (dom, domErrors) = checkAndElaborate(expr.param.tpe, TypedAst.Expr.Sort()(expr.param.tpe.source))
    val sym = LocalSymbol(expr.param.name, dom, ids.freshLocalId())
    val bodyCtx = ctx.withLocal(sym)
    val (cod, codErrors) = checkAndElaborate(expr.body, TypedAst.Expr.Sort()(expr.body.source))(using bodyCtx, metas, ids)
    val piExpr = TypedAst.Expr.Pi(sym, cod, isImplicit = false)(expr.source)
    (piExpr, TypedAst.Expr.Sort()(expr.source), domErrors ++ codErrors)

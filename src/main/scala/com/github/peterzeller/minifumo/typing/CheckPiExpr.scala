package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for pi type expressions. */
object CheckPiExpr:
  /** Infers the sort for a pi type expression. */
  def infer(expr: ast.Expr.Pi)(using ids: IdSupply, ctx: TypeContext): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val localSymbols = ctx.locals.view.mapValues(_.symbol).toMap
    val dom = signatureExpr(expr.param.tpe, ctx.globals, localSymbols)
    val sym = LocalSymbol(expr.param.name, dom, ids.freshLocalId())
    val cod = signatureExpr(expr.body, ctx.globals, localSymbols + (expr.param.name -> sym))
    val piExpr = TypedAst.Expr.Pi(sym, cod, isImplicit = false)(expr.source)
    (piExpr, TypedAst.Expr.Sort()(expr.source), List())

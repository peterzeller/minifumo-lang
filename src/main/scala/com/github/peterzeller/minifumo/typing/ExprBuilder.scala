package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypeChecker.{IdSupply, MetaContext, TypeContext}
import com.github.peterzeller.minifumo.typing.{TypedAst, Symbol}

/** Helpers for building common expressions */
object ExprBuilder:
  def unknownType: TypedAst.Expr =
    TypedAst.Expr.UnknownType()(SourceRange.empty)

  def sort: TypedAst.Expr =
    TypedAst.Expr.Sort()(SourceRange.empty)

  def applyImplicit(f: TypedAst.Expr, args: List[TypedAst.Expr], source: SourceRange): TypedAst.Expr = {
    var res = f
    for arg <- args do {
      val t = res.calculateType.get.asInstanceOf[TypedAst.Expr.Pi].cod
      res = TypedAst.Expr.AppImplicit(res, arg, t)(source)
    }
    res
  }

  def applyExplicit(f: TypedAst.Expr, args: List[TypedAst.Expr], source: SourceRange): TypedAst.Expr = {
    var res = f
    for arg <- args do {
      val t = res.calculateType.get.asInstanceOf[TypedAst.Expr.Pi].cod
      res = TypedAst.Expr.App(res, arg, t)(source)
    }
    res
  }

  def apply(f: TypedAst.Expr, implicitArgs: List[TypedAst.Expr], args: List[TypedAst.Expr], source: SourceRange): TypedAst.Expr = {
    applyExplicit(applyImplicit(f, implicitArgs, source), args, source)
  }

  def v(sym: Symbol, sourceRange: SourceRange): TypedAst.Expr =
    TypedAst.Expr.Var(sym)(sourceRange)

  def equalityConstraint(tpe: TypedAst.Expr, left: TypedAst.Expr, right: TypedAst.Expr, source: SourceRange)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): TypedAst.Expr =
    val eqType = ctx.lookupSymbol("Eq").get
    apply(v(eqType, source), List(tpe), List(left, right), source)


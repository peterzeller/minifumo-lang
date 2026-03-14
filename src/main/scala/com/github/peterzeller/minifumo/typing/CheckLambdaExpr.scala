package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypeChecker.*
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.Sort

/** Type-checking logic for lambda expressions. */
object CheckLambdaExpr:
  /** Infers the type for a lambda expression. */
  def infer(expr: ast.Expr.Lambda)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    var allErrors: List[TypeError] = List()
    val paramType = expr.param.tpe match {
      case Some(t) =>
        val (typedT, errs) = TypeChecker.checkAndElaborate(t, Sort(UniverseLevel.Type1)(SourceRange.empty))
        allErrors ++= errs
        typedT
      case None =>
        // TODO add a meta
        TypedAst.Expr.UnknownType()(expr.source)
    }
    val localSymbol = LocalSymbol(expr.param.name, paramType, ids.freshLocalId())
    val localSymbol2 = LocalSymbol(expr.param.name, paramType, ids.freshLocalId())
    val bodyCtx = ctx.withLocal(localSymbol)
    val (bodyExpr, bodyType, errs) = TypeChecker.infer(expr.body)(using bodyCtx, metas, ids)
    allErrors ++= errs
    val fnType = TypedAst.Expr.Pi(localSymbol2, bodyType, isImplicit = false)(expr.source)
    (TypedAst.Expr.Lambda(localSymbol, bodyExpr, fnType)(expr.source), fnType, allErrors)

  /** Checks a lambda expression against an expected function type. */
  def check(expr: ast.Expr.Lambda, expectedType: TypedAst.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val expectedNorm = whnf(expectedType)
    expectedNorm match
      case TypedAst.Expr.Pi(dom, cod, false) =>
        val p = LocalSymbol(expr.param.name, dom.tpe, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(p)
        val (typedBody, errs) = TypeChecker.check(expr.body, cod)(using bodyCtx, metas, ids)
        (TypedAst.Expr.Lambda(p, typedBody, expectedNorm)(expr.source), errs)
      case _ =>
        checkAndElaborate(expr, expectedType)

package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for lambda expressions. */
object CheckLambdaExpr:
  /** Infers the type for a lambda expression. */
  def infer(
      expr: ast.Expr.Lambda,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      signatureExpr: (ast.Expr, GlobalEnv, Map[String, TypedAst.TermSymbol]) => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val paramType = expr.param.tpe.map(t => signatureExpr(t, ctx.globals, Map())).getOrElse(TypedAst.Expr.UnknownType()(expr.source))
    val localSymbol = TypedAst.LocalSymbol(expr.param.name, paramType, ids.freshLocalId())
    val localSymbol2 = TypedAst.LocalSymbol(expr.param.name, paramType, ids.freshLocalId())
    val bodyCtx = ctx.withLocal(localSymbol)
    val (bodyExpr, bodyType, errs) = inferRec(expr.body, bodyCtx, metas, ids)
    val fnType = TypedAst.Expr.Pi(localSymbol2, bodyType, isImplicit = false)(expr.source)
    (TypedAst.Expr.Lambda(localSymbol, bodyExpr, fnType)(expr.source), fnType, errs)

  /** Checks a lambda expression against an expected function type. */
  def check(
      expr: ast.Expr.Lambda,
      expectedType: TypedAst.Expr,
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      checkAndElaborate: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      whnf: TypedAst.Expr => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val expectedNorm = whnf(expectedType)
    expectedNorm match
      case TypedAst.Expr.Pi(dom, cod, false) =>
        val p = TypedAst.LocalSymbol(expr.param.name, dom.tpe, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(p)
        val (typedBody, errs) = checkRec(expr.body, cod, bodyCtx, metas, ids)
        (TypedAst.Expr.Lambda(p, typedBody, expectedNorm)(expr.source), errs)
      case _ =>
        checkAndElaborate(expr, expectedType, ctx, metas, ids)

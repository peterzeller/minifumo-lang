package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for let expressions. */
object CheckLetExpr:
  /** Infers the type for a let-in expression. */
  def infer(
      expr: ast.Expr.LetIn,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      inferAndElaborateRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      signatureExpr: (ast.Expr, GlobalEnv, Map[String, TypedAst.TermSymbol]) => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val ast.Expr.LetIn(name, declaredType, value, body) = expr
    val inferredValue = declaredType match
      case Some(tpeExpr) =>
        val expected = signatureExpr(tpeExpr, ctx.globals, Map())
        val (typedValue, errs) = checkRec(value, expected, ctx, metas, ids)
        (typedValue, expected, errs)
      case None => inferAndElaborateRec(value, ctx, metas, ids)
    val (valueExpr, valueType, errs) = inferredValue
    val symbol = TypedAst.LocalSymbol(name, valueType, ids.freshLocalId())
    val bodyCtx = ctx.withLocal(symbol, Some(valueExpr))
    val (bodyExpr, bodyType, errs2) = inferRec(body, bodyCtx, metas, ids)
    (TypedAst.Expr.LetIn(symbol, isConstant = false, valueType, valueExpr, bodyExpr)(expr.source), bodyType, errs ++ errs2)

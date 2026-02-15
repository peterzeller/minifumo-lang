package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for let expressions. */
object CheckLetExpr:
  /** Infers the type for a let-in expression. */
  def infer(expr: ast.Expr.LetIn)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val ast.Expr.LetIn(name, declaredType, value, body) = expr
    val inferredValue = declaredType match
      case Some(tpeExpr) =>
        val expected = signatureExpr(tpeExpr, ctx.globals, Map())
        val (typedValue, errs) = TypeChecker.check(value, expected)
        (typedValue, expected, errs)
      case None => TypeChecker.inferAndElaborate(value)
    val (valueExpr, valueType, errs) = inferredValue
    val symbol = TypedAst.LocalSymbol(name, valueType, ids.freshLocalId())
    val bodyCtx = ctx.withLocal(symbol, Some(valueExpr))
    val (bodyExpr, bodyType, errs2) = TypeChecker.infer(body)(using bodyCtx, metas, ids)
    (TypedAst.Expr.LetIn(symbol, isConstant = false, valueType, valueExpr, bodyExpr)(expr.source), bodyType, errs ++ errs2)

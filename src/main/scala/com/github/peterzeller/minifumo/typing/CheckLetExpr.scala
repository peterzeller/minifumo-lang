package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for let expressions. */
object CheckLetExpr:
  /** Checks a let-in expression against an expected body type. */
  def check(expr: ast.Expr.LetIn, expectedType: Option[TypedAst.Expr])(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val ast.Expr.LetIn(name, declaredType, value, body) = expr
    val comment = expr.comment
    val (valueExpr, valueType, errs) = declaredType match
      case Some(tpeExpr) =>
        val (expected, errs1) = TypeChecker.checkAndElaborate(tpeExpr, TypedAst.Expr.Sort(UniverseLevel.Type1)(SourceRange.empty))
        val (typedValue, errs2) = TypeChecker.check(value, expected)
        (typedValue, expected, errs1 ++ errs2)
      case None => TypeChecker.inferAndElaborate(value)
    // add new variable to the context
    val symbol = LocalSymbol(name, valueType, ids.freshLocalId())("")
    
    // there is also a second implicit definition added: an equality proof between the sym and value
    // for let x: Int = 2 + 3, it adds a definition
    // let x_def: Eq[Int](x, 2 + 3)
    val defType = ExprBuilder.equalityConstraint(
      valueType, 
      ExprBuilder.v(symbol, expr.source),
      valueExpr,
      expr.source
    )
    val defSymbol = LocalSymbol(name + "_def", defType, ids.freshMetaId())("")

    val bodyCtx = ctx.withLocal(symbol, Some(valueExpr)).withLocal(defSymbol)

    val (bodyExpr, bodyType, bodyErrs) = expectedType match {
      case Some(expectedType) =>
        val (bodyExpr, errs2) = TypeChecker.check(body, expectedType)(using bodyCtx, metas, ids)
        (bodyExpr, expectedType, errs2)
      case None =>
        TypeChecker.infer(body)(using bodyCtx, metas, ids)
    }
    val typedAst = TypedAst.Expr.LetIn(symbol, isConstant = false, valueType, valueExpr, bodyExpr)(expr.source, comment)
    (typedAst, bodyType, errs ++ bodyErrs)

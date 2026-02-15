package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

import scala.collection.mutable.ListBuffer

/** Type-checking logic for match expressions. */
object CheckMatchExpr:
  /** Infers the result type of a match expression. */
  def infer(
      expr: ast.Expr.Match,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkPattern: (ast.Pattern, TypedAst.Expr, TypeContext, IdSupply) => (TypedAst.Pattern, Map[String, LocalBinding], List[TypeError]),
      isDefEq: (TypedAst.Expr, TypedAst.Expr) => Boolean,
      prettyExpr: TypedAst.Expr => String
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val ast.Expr.Match(scrutinee, cases) = expr
    val (scrutineeExpr, scrutineeType, errs) = inferRec(scrutinee, ctx, metas, ids)
    val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
      val (typedPattern, patternCtx, patternErrors) = checkPattern(pattern, scrutineeType, ctx, ids)
      val caseCtx = ctx.copy(locals = ctx.locals ++ patternCtx)
      val (typedBody, bodyType, bodyErrs) = inferRec(body, caseCtx, metas, ids)
      (typedPattern, typedBody, bodyType, patternErrors ++ bodyErrs)
    }
    val errors = typedCases.flatMap(_._4)
    val typedCasesExpr = typedCases.map { case (pat, bodyExpr, _, _) =>
      TypedAst.MatchCase(pat, bodyExpr)(bodyExpr.source)
    }
    val firstType = typedCases.head._3
    val errs3 = ListBuffer[TypeError]()
    for (_, bodyExpr, caseType, _) <- typedCases.tail do
      if !isDefEq(firstType, caseType) then
        errs3.addOne(TypeError(s"Case should have type ${prettyExpr(firstType)}, but got ${prettyExpr(caseType)}", bodyExpr.source))
    (TypedAst.Expr.Match(scrutineeExpr, typedCasesExpr)(expr.source), firstType, errs ++ errors ++ errs3.toList)

  /** Checks a match expression against an expected type. */
  def check(
      expr: ast.Expr.Match,
      expectedType: TypedAst.Expr,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      checkPattern: (ast.Pattern, TypedAst.Expr, TypeContext, IdSupply) => (TypedAst.Pattern, Map[String, LocalBinding], List[TypeError])
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val ast.Expr.Match(scrutinee, cases) = expr
    val (scrutineeExpr, scrutineeType, errs) = inferRec(scrutinee, ctx, metas, ids)
    val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
      val (typedPattern, patternCtx, patternErrors) = checkPattern(pattern, scrutineeType, ctx, ids)
      val caseCtx = ctx.copy(locals = ctx.locals ++ patternCtx)
      val (typedBody, bodyErrs) = checkRec(body, expectedType, caseCtx, metas, ids)
      (typedPattern, typedBody, patternErrors ++ bodyErrs)
    }
    val errors = typedCases.flatMap(_._3)
    val typedCasesExpr = typedCases.map { case (pat, bodyExpr, _) =>
      TypedAst.MatchCase(pat, bodyExpr)(bodyExpr.source)
    }
    (TypedAst.Expr.Match(scrutineeExpr, typedCasesExpr)(expr.source), errs ++ errors)

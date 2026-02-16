package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

import scala.collection.mutable.ListBuffer

/** Type-checking logic for match expressions. */
object CheckMatchExpr:
  /** Infers the result type of a match expression. */
  def infer(expr: ast.Expr.Match)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val ast.Expr.Match(scrutinee, cases) = expr
    val (scrutineeExpr, scrutineeType, errs) = TypeChecker.infer(scrutinee)
    val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
      val patternResult = checkPattern(pattern, scrutineeType, ctx, ids)
      val caseCtx = applyTypeRefinements(ctx.copy(locals = ctx.locals ++ patternResult.bindings), patternResult.refinements)
      val (typedBody, bodyType, bodyErrs) = TypeChecker.infer(body)(using caseCtx, metas, ids)
      (patternResult.typedPattern, typedBody, bodyType, patternResult.errors ++ bodyErrs)
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
  def check(expr: ast.Expr.Match, expectedType: TypedAst.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val ast.Expr.Match(scrutinee, cases) = expr
    val (scrutineeExpr, scrutineeType, errs) = TypeChecker.infer(scrutinee)
    val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
      val patternResult = checkPattern(pattern, scrutineeType, ctx, ids)
      val caseCtx = applyTypeRefinements(ctx.copy(locals = ctx.locals ++ patternResult.bindings), patternResult.refinements)
      val caseExpectedType = substituteTypeParams(expectedType, patternResult.refinements)
      val (typedBody, bodyErrs) = TypeChecker.check(body, caseExpectedType)(using caseCtx, metas, ids)
      (patternResult.typedPattern, typedBody, patternResult.errors ++ bodyErrs)
    }
    val errors = typedCases.flatMap(_._3)
    val typedCasesExpr = typedCases.map { case (pat, bodyExpr, _) =>
      TypedAst.MatchCase(pat, bodyExpr)(bodyExpr.source)
    }
    (TypedAst.Expr.Match(scrutineeExpr, typedCasesExpr)(expr.source), errs ++ errors)

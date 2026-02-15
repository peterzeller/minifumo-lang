package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Routes expression inference and checking to expression-specific modules. */
object TypeCheckerExprDispatcher:
  /** Infers a typed expression by dispatching on expression kind. */
  def infer(expr: ast.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    expr match
      case v @ ast.Expr.Var(_) => CheckVarExpr.infer(v)
      case l @ ast.Expr.Lit(_) => CheckLiteralExpr.infer(l)
      case c @ ast.Expr.Call(_, _) => CheckCallExpr.infer(c)
      case c @ ast.Expr.CallImplicit(_, _) => CheckCallImplicitExpr.infer(c)
      case l @ ast.Expr.Lambda(_, _) => CheckLambdaExpr.infer(l)
      case l @ ast.Expr.LetIn(_, _, _, _) => CheckLetExpr.infer(l)
      case p @ ast.Expr.Pi(_, _) => CheckPiExpr.infer(p)
      case m @ ast.Expr.Match(_, _) => CheckMatchExpr.infer(m)
      case h @ ast.Expr.Hole() => CheckHoleExpr.infer(h)

  /** Checks an expression by dispatching on expression kind. */
  def check(expr: ast.Expr, expectedType: TypedAst.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    expr match
      case l @ ast.Expr.Lambda(_, _) => CheckLambdaExpr.check(l, expectedType)
      case m @ ast.Expr.Match(_, _) => CheckMatchExpr.check(m, expectedType)
      case _ => checkAndElaborate(expr, expectedType)

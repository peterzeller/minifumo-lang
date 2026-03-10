package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Routes expression inference and checking to expression-specific modules. */
object TypeCheckerExprDispatcher:
  /** Infers a typed expression by dispatching on expression kind. */
  def infer(expr: ast.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) = {
    try
      expr match
        case v @ ast.Expr.Var(_) => CheckVarExpr.infer(v)
        case l @ ast.Expr.Lit(_) => CheckLiteralExpr.infer(l)
        case c @ ast.Expr.Call(_, _) => CheckCallExpr.infer(c)
        case c @ ast.Expr.CallImplicit(_, _) => CheckCallImplicitExpr.infer(c)
        case l @ ast.Expr.Lambda(_, _) => CheckLambdaExpr.infer(l)
        case l @ ast.Expr.LetIn(_, _, _, _) => CheckLetExpr.check(l, None)
        case f @ ast.Expr.FieldAccess(_, _) => CheckFieldAccessExpr.infer(f)
        case p @ ast.Expr.Pi(_, _) => CheckPiExpr.infer(p)
        case m @ ast.Expr.Match(_, _, _) => CheckMatchExpr.infer(m)
        case h @ ast.Expr.Hole() => CheckHoleExpr.infer(h)
        case a @ ast.Expr.Axiom() => CheckAxiomExpr.infer(a)
    catch
      case e: Exception =>
        throw new RuntimeException(s"error inferring type of $expr", e)
  }

  /** Checks an expression by dispatching on expression kind. */
  def check(expr: ast.Expr, expectedType: TypedAst.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) = {
    try
      expr match
        case l @ ast.Expr.Lambda(_, _) => CheckLambdaExpr.check(l, expectedType)
        case l @ ast.Expr.LetIn(_, _, _, _) => 
          val (a, _, es) = CheckLetExpr.check(l, Some(expectedType))
          (a, es)
        case m @ ast.Expr.Match(_, _, _) => CheckMatchExpr.check(m, expectedType)
        case a @ ast.Expr.Axiom() => CheckAxiomExpr.check(a, expectedType)
        case _ => checkAndElaborate(expr, expectedType)
    catch
      case e: Exception =>
        throw new RuntimeException(s"error checking type of $expr", e)
  }

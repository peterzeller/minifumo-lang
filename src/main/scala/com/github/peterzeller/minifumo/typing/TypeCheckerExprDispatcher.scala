package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Routes expression inference and checking to expression-specific modules. */
object TypeCheckerExprDispatcher:
  /** Infers a typed expression by dispatching on expression kind. */
  def infer(
      expr: ast.Expr,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      inferAndElaborateRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      checkPattern: (ast.Pattern, TypedAst.Expr, TypeContext, IdSupply) => (TypedAst.Pattern, Map[String, LocalBinding], List[TypeError]),
      signatureExpr: (ast.Expr, GlobalEnv, Map[String, TypedAst.TermSymbol]) => TypedAst.Expr,
      substitute: (TypedAst.Expr, TypedAst.LocalSymbol, TypedAst.Expr) => TypedAst.Expr,
      literalType: (ast.Literal, TypeContext) => TypedAst.Expr,
      whnf: TypedAst.Expr => TypedAst.Expr,
      isDefEq: (TypedAst.Expr, TypedAst.Expr) => Boolean,
      prettyExpr: TypedAst.Expr => String,
      freshMeta: (String, TypedAst.Expr, ast.SourceRange) => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    expr match
      case v @ ast.Expr.Var(_) => CheckVarExpr.infer(v)
      case l @ ast.Expr.Lit(_) => CheckLiteralExpr.infer(l, literalType)
      case c @ ast.Expr.Call(_, _) => CheckCallExpr.infer(c, inferRec, inferAndElaborateRec, checkRec, whnf, substitute)
      case c @ ast.Expr.CallImplicit(_, _) => CheckCallImplicitExpr.infer(c, inferRec, checkRec, whnf, substitute)
      case l @ ast.Expr.Lambda(_, _) => CheckLambdaExpr.infer(l, inferRec, signatureExpr)
      case l @ ast.Expr.LetIn(_, _, _, _) => CheckLetExpr.infer(l, inferRec, inferAndElaborateRec, checkRec, signatureExpr)
      case p @ ast.Expr.Pi(_, _) => CheckPiExpr.infer(p, signatureExpr)
      case m @ ast.Expr.Match(_, _) => CheckMatchExpr.infer(m, inferRec, checkPattern, isDefEq, prettyExpr)
      case h @ ast.Expr.Hole() => CheckHoleExpr.infer(h, freshMeta)

  /** Checks an expression by dispatching on expression kind. */
  def check(
      expr: ast.Expr,
      expectedType: TypedAst.Expr,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      checkAndElaborate: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      checkPattern: (ast.Pattern, TypedAst.Expr, TypeContext, IdSupply) => (TypedAst.Pattern, Map[String, LocalBinding], List[TypeError]),
      whnf: TypedAst.Expr => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    expr match
      case l @ ast.Expr.Lambda(_, _) => CheckLambdaExpr.check(l, expectedType, checkRec, checkAndElaborate, whnf)
      case m @ ast.Expr.Match(_, _) => CheckMatchExpr.check(m, expectedType, inferRec, checkRec, checkPattern)
      case _ => checkAndElaborate(expr, expectedType, ctx, metas, ids)

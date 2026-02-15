package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.UnknownType
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for explicit call expressions. */
object CheckCallExpr:
  /** Infers the type for an explicit function call. */
  def infer(
      expr: ast.Expr.Call,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      inferAndElaborateRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      whnf: TypedAst.Expr => TypedAst.Expr,
      substitute: (TypedAst.Expr, TypedAst.LocalSymbol, TypedAst.Expr) => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (typedCallee, calleeType, errs1) = inferAndElaborateRec(expr.callee, ctx, metas, ids)
    whnf(calleeType) match
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        val (typedArg, errs2) = checkRec(expr.arg, dom.tpe, ctx, metas, ids)
        val resultType = substitute(cod, dom, typedArg)
        val errs3 =
          if !isImplicit then List()
          else List(TypeError("Expected an implicit function argument", expr.callee.source))
        (TypedAst.Expr.App(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)
      case other =>
        val (typedArg, _, errs2) = inferRec(expr.arg, ctx, metas, ids)
        val resultType = TypedAst.Expr.UnknownType()(SourceRange.empty)
        val errs3 =
          if other.isInstanceOf[UnknownType] then List()
          else List(TypeError(s"Expected a function but found expression of type $other", expr.callee.source))
        (TypedAst.Expr.App(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)

/** Type-checking logic for implicit call expressions. */
object CheckCallImplicitExpr:
  /** Infers the type for an implicit function call. */
  def infer(
      expr: ast.Expr.CallImplicit,
      inferRec: (ast.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, TypedAst.Expr, List[TypeError]),
      checkRec: (ast.Expr, TypedAst.Expr, TypeContext, MetaContext, IdSupply) => (TypedAst.Expr, List[TypeError]),
      whnf: TypedAst.Expr => TypedAst.Expr,
      substitute: (TypedAst.Expr, TypedAst.LocalSymbol, TypedAst.Expr) => TypedAst.Expr
    )(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (typedCallee, calleeType, errs1) = inferRec(expr.callee, ctx, metas, ids)
    whnf(calleeType) match
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        val (typedArg, errs2) = checkRec(expr.arg, dom.tpe, ctx, metas, ids)
        val resultType = substitute(cod, dom, typedArg)
        val errs3 =
          if isImplicit then List()
          else List(TypeError("Expected an explicit function argument", expr.callee.source))
        (TypedAst.Expr.AppImplicit(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)
      case other =>
        val (typedArg, _, errs2) = inferRec(expr.arg, ctx, metas, ids)
        val resultType = TypedAst.Expr.UnknownType()(SourceRange.empty)
        val errs3 =
          if other.isInstanceOf[UnknownType] then List()
          else List(TypeError(s"Expected a function but found expression of type $other", expr.callee.source))
        (TypedAst.Expr.AppImplicit(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)

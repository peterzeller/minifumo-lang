package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.UnknownType
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for explicit call expressions. */
object CheckCallExpr:
  /** Infers the type for an explicit function call. */
  def infer(expr: ast.Expr.Call)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (typedCallee, calleeType, errs1) = TypeChecker.inferAndElaborate(expr.callee)
    whnf(calleeType) match
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        val (typedArg, errs2) = TypeChecker.check(expr.arg, dom.tpe)
        val resultType = substitute(cod, dom, typedArg)
        val deferredMetaArgError =
          if CallMetaUtils.hasNonPatternMetaApplication(TypeChecker.instantiate(dom.tpe)) then
            List(TypeError("Could not infer implicit argument for call parameter type", expr.arg.source))
          else
            List()
        val errs3 =
          if !isImplicit then List()
          else List(TypeError("Expected an implicit function argument", expr.callee.source))
        (TypedAst.Expr.App(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3 ++ deferredMetaArgError)
      case other =>
        val (typedArg, _, errs2) = TypeChecker.infer(expr.arg)
        val resultType = TypedAst.Expr.UnknownType()(SourceRange.empty)
        val errs3 =
          if other.isInstanceOf[UnknownType] then List()
          else List(TypeError(s"Expected a function but found expression of type $other\nCallee: ${expr.callee}\nArg: ${expr.arg}", expr.callee.source))
        (TypedAst.Expr.App(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)

/** Type-checking logic for implicit call expressions. */
object CheckCallImplicitExpr:
  /** Infers the type for an implicit function call. */
  def infer(expr: ast.Expr.CallImplicit)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (typedCallee, calleeType, errs1) = TypeChecker.infer(expr.callee)
    whnf(calleeType) match
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        val (typedArg, errs2) = TypeChecker.check(expr.arg, dom.tpe)
        val resultType = substitute(cod, dom, typedArg)
        val deferredMetaArgError =
          if CallMetaUtils.hasNonPatternMetaApplication(TypeChecker.instantiate(dom.tpe)) then
            List(TypeError("Could not infer implicit argument for call parameter type", expr.arg.source))
          else
            List()
        val errs3 =
          if isImplicit then List()
          else List(TypeError(s"Expected an explicit function argument\nCallee ${expr.callee} has type ${calleeType}", expr.callee.source))
        (TypedAst.Expr.AppImplicit(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3 ++ deferredMetaArgError)
      case other =>
        val (typedArg, _, errs2) = TypeChecker.infer(expr.arg)
        val resultType = TypedAst.Expr.UnknownType()(SourceRange.empty)
        val errs3 =
          if other.isInstanceOf[UnknownType] then List()
          else List(TypeError(s"Expected a function but found expression of type $other\nCallee: ${expr.callee}\nArg: ${expr.arg}", expr.callee.source))
        (TypedAst.Expr.AppImplicit(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)

/** Shared helpers for call-expression type checking. */
private object CallMetaUtils:
  /** Detects non-pattern meta applications that cannot be validated during argument checking. */
  def hasNonPatternMetaApplication(term: TypedAst.Expr): Boolean =
    def loop(current: TypedAst.Expr): Boolean =
      current match
        case TypedAst.Expr.App(callee, _, _) =>
          val (head, args) = decompose(current)
          val hasNonLocalArg = args.exists {
            case TypedAst.Expr.Var(_: LocalSymbol) => false
            case _ => true
          }
          (head.isInstanceOf[TypedAst.Expr.Meta] && hasNonLocalArg) || loop(callee)
        case TypedAst.Expr.AppImplicit(callee, _, _) =>
          val (head, args) = decompose(current)
          val hasNonLocalArg = args.exists {
            case TypedAst.Expr.Var(_: LocalSymbol) => false
            case _ => true
          }
          (head.isInstanceOf[TypedAst.Expr.Meta] && hasNonLocalArg) || loop(callee)
        case TypedAst.Expr.Lambda(_, body, tpe) => loop(body) || loop(tpe)
        case TypedAst.Expr.Pi(dom, cod, _) => loop(dom.tpe) || loop(cod)
        case TypedAst.Expr.LetIn(_, _, declaredType, value, body) => loop(declaredType) || loop(value) || loop(body)
        case TypedAst.Expr.Match(scrutinee, motive, cases) => loop(scrutinee) || loop(motive) || cases.exists(c => loop(c.body))
        case _ => false
    def decompose(expr: TypedAst.Expr): (TypedAst.Expr, List[TypedAst.Expr]) =
      expr match
        case TypedAst.Expr.App(callee, arg, _) =>
          val (head, args) = decompose(callee)
          (head, args :+ arg)
        case TypedAst.Expr.AppImplicit(callee, arg, _) =>
          val (head, args) = decompose(callee)
          (head, args :+ arg)
        case other =>
          (other, List())
    loop(term)

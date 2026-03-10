package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for match expressions. */
object CheckMatchExpr:
  /** Infers the result type of a match expression by delegating to check-mode with a meta expected type. */
  def infer(expr: ast.Expr.Match)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val unknownType = freshMeta("matchResult", TypedAst.Expr.Sort()(expr.source), expr.source)
    val (typedExpr, errs) = check(expr, unknownType)
    (typedExpr, instantiate(unknownType), errs)

  /** Checks a match expression against an expected type using dependent branch refinement. */
  def check(expr: ast.Expr.Match, expectedType: TypedAst.Expr)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val ast.Expr.Match(scrutinee, cases) = expr
    val (scrutineeExpr, scrutineeType, scrutineeErrs) = TypeChecker.infer(scrutinee)
    val branchFactName = inferBranchFactName(expr.source, cases)
    val motiveParam = LocalSymbol("x_scrut", scrutineeType, ids.freshLocalId())
    val motiveBody = replaceExpr(expectedType, scrutineeExpr, TypedAst.Expr.Var(motiveParam)(expr.source))
    val motive = TypedAst.Expr.Lambda(motiveParam, motiveBody, TypedAst.Expr.UnknownType()(expr.source))(expr.source)
    val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
      val patternResult = checkPattern(pattern, scrutineeType, ctx, ids)
      val patternTerm = patternToExpr(patternResult.typedPattern, patternResult.refinements, scrutineeExpr.source)
      val equalityFactType = ExprBuilder.equalityConstraint(scrutineeType, scrutineeExpr, patternTerm, pattern.source)
      val equalityFact = LocalSymbol(branchFactName, equalityFactType, ids.freshLocalId())
      val caseCtx0 = ctx.copy(locals = ctx.locals ++ patternResult.bindings)
      val caseCtx = applyTypeRefinements(caseCtx0.withLocal(equalityFact), patternResult.refinements)
      val branchExpected0 = TypedAst.Expr.App(motive, patternTerm, TypedAst.Expr.UnknownType()(body.source))(body.source)
      val branchExpectedType = whnf(substituteTypeParams(branchExpected0, patternResult.refinements))
      val (typedBody, bodyErrs) = TypeChecker.check(body, branchExpectedType)(using caseCtx, metas, ids)
      val caseSource = pattern.source.merge(body.source)
      (TypedAst.MatchCase(patternResult.typedPattern, typedBody)(caseSource), patternResult.errors ++ bodyErrs)
    }
    val typedMatch = TypedAst.Expr.Match(scrutineeExpr, motive, typedCases.map(_._1))(expr.source)
    val errors = typedCases.flatMap(_._2)
    (typedMatch, scrutineeErrs ++ errors)

  /** Chooses default names for branch equality facts. */
  private def inferBranchFactName(source: ast.SourceRange, cases: List[ast.MatchCase]): String =
    if isIfDesugaredMatch(source, cases) then "if_Eq" else "match_Eq"

  /** Detects parser-desugared boolean if expressions represented as a two-case match. */
  private def isIfDesugaredMatch(source: ast.SourceRange, cases: List[ast.MatchCase]): Boolean =
    def isCtor0Pattern(pattern: ast.Pattern, name: String): Boolean =
      pattern match
        case ast.Pattern.BinderOrCtor0(patternName) => patternName == name
        case ast.Pattern.Ctor(patternName, args) => patternName == name && args.isEmpty
        case _ => false

    cases match
      case List(ast.MatchCase(truePattern, _), ast.MatchCase(falsePattern, _)) =>
        val parserGeneratedPatternRanges = truePattern.source == source && falsePattern.source == source
        parserGeneratedPatternRanges && isCtor0Pattern(truePattern, "True") && isCtor0Pattern(falsePattern, "False")
      case _ => false

  /** Converts a typed pattern into a branch refinement term. */
  private def patternToExpr(pattern: TypedAst.Pattern, refinements: Map[Int, TypedAst.Expr], source: ast.SourceRange): TypedAst.Expr =
    pattern match
      case TypedAst.Pattern.Wildcard() => TypedAst.Expr.UnknownType()(source)
      case TypedAst.Pattern.Lit(value) => TypedAst.Expr.Lit(value)(source)
      case TypedAst.Pattern.Binder(symbol) => TypedAst.Expr.Var(symbol)(source)
      case TypedAst.Pattern.Ctor(symbol, args) =>
        val ctorDecl = symbol.dt.typed.ctors.find(_.symbol.name == symbol.name) match {
          case Some(v) => v
          case None =>
            // if constructor is not found, return unknown type
            return TypedAst.Expr.UnknownType()(source)
        }
        val withImplicitArgs = ctorDecl.implicitFields.foldLeft[TypedAst.Expr](TypedAst.Expr.Var(symbol)(source)) { (callee, field) =>
          val implicitArg = refinements.getOrElse(field.id, TypedAst.Expr.Var(field)(source))
          TypedAst.Expr.AppImplicit(callee, implicitArg, TypedAst.Expr.UnknownType()(source))(source)
        }
        args.foldLeft[TypedAst.Expr](withImplicitArgs) { (callee, argPattern) =>
          TypedAst.Expr.App(callee, patternToExpr(argPattern, refinements, source), TypedAst.Expr.UnknownType()(source))(source)
        }

  /** Replaces exact occurrences of a target expression with a replacement expression. */
  private def replaceExpr(term: TypedAst.Expr, target: TypedAst.Expr, replacement: TypedAst.Expr): TypedAst.Expr =
    if term == target then
      replacement
    else
      term match
        case TypedAst.Expr.App(callee, arg, tpe) =>
          TypedAst.Expr.App(replaceExpr(callee, target, replacement), replaceExpr(arg, target, replacement), replaceExpr(tpe, target, replacement))(term.source)
        case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
          TypedAst.Expr.AppImplicit(replaceExpr(callee, target, replacement), replaceExpr(arg, target, replacement), replaceExpr(tpe, target, replacement))(term.source)
        case TypedAst.Expr.Lambda(param, body, tpe) =>
          TypedAst.Expr.Lambda(param, replaceExpr(body, target, replacement), replaceExpr(tpe, target, replacement))(term.source)
        case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
          TypedAst.Expr.LetIn(symbol, isConstant, replaceExpr(declaredType, target, replacement), replaceExpr(value, target, replacement), replaceExpr(body, target, replacement))(term.source)
        case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
          TypedAst.Expr.Pi(dom, replaceExpr(cod, target, replacement), isImplicit)(term.source)
        case TypedAst.Expr.Match(scrutinee, motive, cases) =>
          val rewrittenCases = cases.map(c => TypedAst.MatchCase(c.pattern, replaceExpr(c.body, target, replacement))(c.source))
          TypedAst.Expr.Match(replaceExpr(scrutinee, target, replacement), replaceExpr(motive, target, replacement), rewrittenCases)(term.source)
        case _ => term

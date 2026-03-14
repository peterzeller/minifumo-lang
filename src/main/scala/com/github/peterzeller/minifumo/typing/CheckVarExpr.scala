package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.typing.TypedAst.Expr
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for variable expressions. */
object CheckVarExpr:
  /** Infers the type for a variable expression. */
  def infer(expr: ast.Expr.Var)(using ctx: TypeContext): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    expr match
      case ast.Expr.Var("Prop") =>
        val propSort = TypedAst.Expr.Sort(UniverseLevel.Prop)(expr.source)
        val typeSort = TypedAst.Expr.Sort(UniverseLevel.Type1)(expr.source)
        (propSort, typeSort, List())
      case ast.Expr.Var("Type") =>
        val inferredLevel =
          if ctx.lookupSymbol("u").isDefined then UniverseLevel.Generic("u")
          else UniverseLevel.Type1
        val typeSort = TypedAst.Expr.Sort(inferredLevel)(expr.source)
        (typeSort, TypedAst.Expr.Sort(UniverseLevel.Type1)(expr.source), List())
      case ast.Expr.Var(name) =>
        ctx.lookupSymbol(name) match
          case Some(symbol) =>
            (TypedAst.Expr.Var(symbol)(expr.source), symbol.tpe, List())
          case None =>
            val errs = List(TypeError(s"Unknown symbol ${name}", expr.source))
            val unknownType = Expr.UnknownType()(SourceRange.empty)
            (TypedAst.Expr.Var(ErrorSymbol(name, unknownType))(expr.source), unknownType, errs)

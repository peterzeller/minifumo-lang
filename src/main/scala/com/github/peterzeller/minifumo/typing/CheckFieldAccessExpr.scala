package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.typing.TypeChecker.*

/** Type-checking logic for field access expressions. */
object CheckFieldAccessExpr:
  /** Infers the type for a field access by desugaring to a generated accessor function call. */
  def infer(expr: ast.Expr.FieldAccess)(using ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (typedReceiver, receiverType, receiverErrors) = TypeChecker.inferAndElaborate(expr.receiver)
    resolveDatatypeSymbol(receiverType) match
      case Some(dtSymbol) =>
        val accessorName = s"${dtSymbol.name}_${expr.fieldName}"
        ctx.globals.names.get(accessorName) match
          case Some(_) =>
            val accessorExpr = ast.Expr.Var(accessorName)(expr.source)
            val desugaredExpr = ast.Expr.Call(accessorExpr, expr.receiver)(expr.source)
            val (typedExpr, inferredType, callErrors) = TypeChecker.inferAndElaborate(desugaredExpr)
            (typedExpr, inferredType, receiverErrors ++ callErrors)
          case None =>
            (
              TypedAst.Expr.App(
                TypedAst.Expr.Var(ErrorSymbol(accessorName, TypedAst.Expr.UnknownType()(expr.source)))(expr.source),
                typedReceiver,
                TypedAst.Expr.UnknownType()(expr.source)
              )(expr.source),
              TypedAst.Expr.UnknownType()(expr.source),
              receiverErrors :+ TypeError(s"Type ${dtSymbol.name} has no accessible field ${expr.fieldName}", expr.source)
            )
      case None =>
        (
          typedReceiver,
          TypedAst.Expr.UnknownType()(expr.source),
          receiverErrors :+ TypeError(s"Cannot resolve field access .${expr.fieldName} because receiver type is not a known single-constructor datatype", expr.source)
        )

  /** Extracts the datatype symbol from the head of a type expression. */
  private def resolveDatatypeSymbol(tpe: TypedAst.Expr)(using ctx: Context, metas: MetaContext): Option[DatatypeSymbol] =
    def head(expr: TypedAst.Expr): Option[Symbol] =
      whnf(expr) match
        case TypedAst.Expr.App(callee, _, _) => head(callee)
        case TypedAst.Expr.AppImplicit(callee, _, _) => head(callee)
        case TypedAst.Expr.Var(symbol) => Some(symbol)
        case _ => None

    head(tpe) match
      case Some(dt: DatatypeSymbol) if dt.typed.ctors.length == 1 => Some(dt)
      case _ => None

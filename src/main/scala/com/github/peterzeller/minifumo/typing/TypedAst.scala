package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Pi, Sort, UnknownType}

import java.nio.file.Path

object TypedAst:
  case class Program(items: List[TopLevel])(val source: SourceRange)

  enum TopLevel:
    case DataDecl(symbol: DatatypeSymbol, typeParams: List[LocalSymbol], ctors: List[CtorDecl])(val source: SourceRange)
    case FunDecl(
        sig: FunSig,
        body: Expr
      )(val source: SourceRange)

  case class FunSig(
    symbol: FunctionSymbol,
    typeParams: List[LocalSymbol],
    params: List[LocalSymbol],
    returnType: Expr,
  ):
    /** Calculate the function type of the function signature */
    lazy val functionType: Expr =
      var r = returnType
      for p <- params.reverseIterator do
        r = Pi(p, r, false)(r.source)
      for p <- typeParams.reverseIterator do
        r = Pi(p, r, true)(r.source)
      r
      

  final case class CtorDecl(symbol: CtorSymbol, fields: List[CtorField])(val source: SourceRange)
  final case class CtorField(name: String, tpe: Expr)(val source: SourceRange)

  enum Expr:
    def source: SourceRange

    case Lit(value: ast.Literal)(val source: SourceRange)
    case Var(symbol: Symbol)(val source: SourceRange)
    case AppImplicit(callee: Expr, arg: Expr, tpe: Expr)(val source: SourceRange)
    case App(callee: Expr, arg: Expr, tpe: Expr)(val source: SourceRange)
    case Pi(dom: LocalSymbol, cod: Expr, isImplicit: Boolean)(val source: SourceRange)
    case Sort()(val source: SourceRange)

    case Lambda(param: LocalSymbol, body: Expr, tpe: Expr)(val source: SourceRange)
    case LetIn(
        symbol: LocalSymbol,
        isConstant: Boolean,
        declaredType: Expr,
        value: Expr,
        body: Expr,
      )(val source: SourceRange)

    case Meta(index: Int, tpe: Expr)(val name: String, val source: SourceRange)

    // placeholder when typing is unknown
    case UnknownType()(val source: SourceRange)
    case Match(scrutinee: Expr, motive: Expr, cases: List[MatchCase])(val source: SourceRange)

  final case class MatchCase(pattern: Pattern, body: Expr)(val source: SourceRange)

  enum Pattern:
    case Wildcard()(val source: SourceRange)
    case Lit(value: ast.Literal)(val source: SourceRange)
    case Binder(symbol: LocalSymbol)(val source: SourceRange)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])(val source: SourceRange)

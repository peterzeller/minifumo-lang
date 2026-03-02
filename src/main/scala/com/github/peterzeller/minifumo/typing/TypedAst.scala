package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Sort, UnknownType}

import java.nio.file.Path

object TypedAst:
  sealed trait Symbol:
    def name: String
    def tpe: Expr

  sealed trait TermSymbol extends Symbol

  final case class LocalSymbol(name: String, tpe: Expr, id: Int) extends TermSymbol
  final case class BuiltinValueSymbol(name: String, tpe: Expr) extends TermSymbol // TODO do we need this?
  final case class ErrorSymbol(name: String, tpe: Expr) extends Symbol
  final case class DatatypeSymbol(sym: GlobalSymbol, tpe: Expr) extends Symbol:
    def name: String = sym.name

  final case class FunctionSymbol(sym: GlobalSymbol, tpe: Expr) extends Symbol:
    def name: String = sym.name
  final case class CtorSymbol(sym: GlobalSymbol, tpe: Expr) extends Symbol
    :
    def name: String = sym.name

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
  )

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

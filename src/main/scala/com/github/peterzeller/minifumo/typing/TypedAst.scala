package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange

object TypedAst:
  sealed trait Symbol:
    def name: String
    def tpe: Expr

  sealed trait TermSymbol extends Symbol

  final case class LocalSymbol(name: String, tpe: Expr, id: Int) extends TermSymbol
  final case class ParamSymbol(name: String, tpe: Expr, id: Int) extends TermSymbol
  final case class BuiltinValueSymbol(name: String, tpe: Expr) extends TermSymbol
  final case class ErrorSymbol(name: String, tpe: Expr) extends Symbol

  final case class FunctionSymbol(name: String, tpe: Expr) extends Symbol
  final case class CtorSymbol(name: String, tpe: Expr) extends Symbol
  case class Program(items: List[TopLevel])(val source: SourceRange)

  enum TopLevel:
    case DataDecl(name: String, typeParams: List[String], ctors: List[CtorDecl])(val source: SourceRange)
    case FunDecl(
        symbol: FunctionSymbol,
        typeParams: List[String],
        params: List[ParamSymbol],
        body: Expr
      )(val source: SourceRange)

  final case class CtorDecl(symbol: CtorSymbol, fields: List[CtorField])(val source: SourceRange)
  final case class CtorField(name: String, tpe: Expr)(val source: SourceRange)

  enum Expr:
    def source: SourceRange

    case Lit(value: ast.Literal)(val source: SourceRange)
    case Var(symbol: Symbol)(val source: SourceRange)
    case App(callee: Expr, args: Expr, tpe: Expr)(val source: SourceRange)
    case Pi(dom: Expr, cod: Expr)(val source: SourceRange)
    case Sort()(val source: SourceRange)

    case Lambda(param: LocalSymbol, body: Expr, tpe: Expr)(val source: SourceRange)
    case LetIn(
        symbol: LocalSymbol,
        isConstant: Boolean,
        declaredType: Expr,
        value: Expr,
        body: Expr,
      )(val source: SourceRange)

    case Meta(index: Int, tpe: Expr)(val source: SourceRange)

    // placeholder when typing is unknown
    case UnknownType(tpe: Expr)(val source: SourceRange)

  class MatchCase(pattern: Pattern, body: Expr)(val source: SourceRange)

  enum Pattern:
    case Wildcard()(val source: SourceRange)
    case Lit(value: ast.Literal)(val source: SourceRange)
    case Binder(symbol: LocalSymbol)(val source: SourceRange)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])(val source: SourceRange)

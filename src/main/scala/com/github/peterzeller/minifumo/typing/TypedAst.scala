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

  final case class FunctionSymbol(name: String, typeParams: List[String], tpe: Expr) extends Symbol
  final case class CtorSymbol(name: String, typeParams: List[String], tpe: Expr, arity: Int, resultType: Expr)
      extends Symbol
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

  sealed trait Expr:
    def tpe: Expr
    def source: SourceRange

  object Expr:
    final case class Lit(value: ast.Literal, tpe: Expr)(val source: SourceRange) extends Expr
    final case class Var(symbol: Symbol, tpe: Expr)(val source: SourceRange) extends Expr
    final case class CallFun(callee: Expr, args: Expr, tpe: Expr)(val source: SourceRange)
        extends Expr
    final case class CallCtor(symbol: CtorSymbol, args: List[Expr], tpe: Expr)(val source: SourceRange) extends Expr
    final case class Lambda(param: LocalSymbol, body: Expr, tpe: Expr)(val source: SourceRange) extends Expr
    final case class LetIn(
        symbol: LocalSymbol,
        isConstant: Boolean,
        declaredType: Option[Expr],
        value: Expr,
        body: Expr,
        tpe: Expr
      )(val source: SourceRange) extends Expr

  final case class MatchCase(pattern: Pattern, body: Expr)(val source: SourceRange)

  enum Pattern:
    case Wildcard()(val source: SourceRange)
    case Lit(value: ast.Literal)(val source: SourceRange)
    case Binder(symbol: LocalSymbol)(val source: SourceRange)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])(val source: SourceRange)

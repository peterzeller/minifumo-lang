package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast

object TypedAst:
  enum Type:
    case Name(value: String)
    case App(base: Type, args: List[Type])
    case Fun(params: List[Type], result: Type)
    case Unknown

  sealed trait Symbol:
    def name: String
    def tpe: Type

  sealed trait TermSymbol extends Symbol

  final case class LocalSymbol(name: String, tpe: Type, id: Int) extends TermSymbol
  final case class ParamSymbol(name: String, tpe: Type, id: Int) extends TermSymbol
  final case class BuiltinValueSymbol(name: String, tpe: Type) extends TermSymbol
  final case class ErrorSymbol(name: String, tpe: Type) extends Symbol

  final case class FunctionSymbol(name: String, tpe: Type.Fun) extends Symbol
  final case class CtorSymbol(name: String, tpe: Type.Fun, arity: Int, resultType: Type) extends Symbol
  final case class BuiltinFunctionSymbol(name: String, tpe: Type.Fun) extends Symbol

  case class Program(items: List[TopLevel])

  enum TopLevel:
    case DataDecl(name: String, typeParams: List[String], ctors: List[CtorDecl])
    case FunDecl(symbol: FunctionSymbol, typeParams: List[String], params: List[ParamSymbol], body: Suite)

  final case class CtorDecl(symbol: CtorSymbol, fields: List[CtorField])
  final case class CtorField(name: String, tpe: Type)

  enum Suite:
    case Block(exprs: List[Expr], tpe: Type)
    case Single(expr: Expr)

  sealed trait Expr:
    def tpe: Type

  object Expr:
    final case class Lit(value: ast.Literal, tpe: Type) extends Expr
    final case class Var(symbol: Symbol, tpe: Type) extends Expr
    final case class Paren(expr: Expr, tpe: Type) extends Expr
    final case class Call(callee: Expr, args: List[Expr], tpe: Type) extends Expr
    final case class LetIn(
        symbol: LocalSymbol,
        isConstant: Boolean,
        declaredType: Option[Type],
        value: Expr,
        body: Expr,
        tpe: Type
      ) extends Expr
    final case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr, tpe: Type) extends Expr
    final case class For(symbol: LocalSymbol, inExpr: Expr, body: Suite, tpe: Type) extends Expr
    final case class While(cond: Expr, body: Suite, tpe: Type) extends Expr
    final case class Match(scrutinee: Expr, cases: List[MatchCase], tpe: Type) extends Expr
    final case class Return(expr: Expr, tpe: Type) extends Expr

  final case class MatchCase(pattern: Pattern, body: Suite)

  enum Pattern:
    case Wildcard
    case Lit(value: ast.Literal)
    case Binder(symbol: LocalSymbol)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])

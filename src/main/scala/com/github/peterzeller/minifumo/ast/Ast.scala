package com.github.peterzeller.minifumo.ast

case class Program(items: List[TopLevel])

enum TopLevel:
  case DataDecl(name: String, typeParams: List[String], ctors: List[CtorDecl])
  case FunDecl(
      name: String,
      typeParams: List[String],
      params: List[FunParam],
      returnType: Option[Type],
      body: Suite
    )

final case class CtorDecl(name: String, fields: List[CtorField])
final case class CtorField(name: String, tpe: Type)

final case class FunParam(name: String, tpe: Type)

enum Suite:
  case Block(exprs: List[Expr])
  case Single(expr: Expr)

enum Type:
  case Name(value: String)
  case Paren(inner: Type)
  case App(base: Type, args: List[Type])

enum Expr:
  case Lit(value: Literal)
  case Var(name: String)
  case Paren(expr: Expr)
  // Call is used for both function calls, as well as expressions like "a + b"
  // For example, `a+b` is represented as `Call(Var("+"), List(Var("a"), Var("b")))`
  case Call(callee: Expr, args: List[Expr])
  // Let and Var bindings
  case LetIn(name: String, isConstant: Boolean, tpe: Option[Type], value: Expr, body: Expr)
  case IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr)
  case For(name: String, inExpr: Expr, body: Suite)
  case While(cond: Expr, body: Suite)
  case Match(scrutinee: Expr, cases: List[MatchCase])
  case Return(expr: Expr)

final case class MatchCase(pattern: Pattern, body: Suite)

enum Literal:
  case IntLit(value: String)
  case BoolLit(value: Boolean)
  case StringLit(value: String)

enum Pattern:
  case Wildcard
  case Lit(value: Literal)
  case BinderOrCtor0(name: String)
  case Ctor(name: String, args: List[Pattern])

package com.github.peterzeller.minifumo.ast

case class SourcePos(line: Int, column: Int)
case class SourceRange(start: SourcePos, end: SourcePos)

case class ProgramFile(items: List[TopLevel])(val source: SourceRange)

enum TopLevel:
  case DataDecl(name: String, typeParams: List[String], ctors: List[CtorDecl])(val source: SourceRange)
  case FunDecl(
      name: String,
      typeParams: List[String],
      params: List[FunParam],
      returnType: Option[Type],
      givenParams: List[FunParam],
      body: Suite
    )(val source: SourceRange)
  case TypeClassDecl(
      name: String,
      typeParams: List[String],
      members: List[FunSig]
    )(val source: SourceRange)
  case InstanceDecl(
      name: String,
      typeParams: List[String],
      head: Type,
      givenParams: List[FunParam],
      members: List[TopLevel.FunDecl]
    )(val source: SourceRange)

  def source: SourceRange

final case class FunSig(
    name: String,
    typeParams: List[String],
    params: List[FunParam],
    returnType: Option[Type],
    givenParams: List[FunParam]
  )(val source: SourceRange)

final case class CtorDecl(name: String, fields: List[CtorField])(val source: SourceRange)
final case class CtorField(name: String, tpe: Type)(val source: SourceRange)

final case class FunParam(name: String, tpe: Type)(val source: SourceRange)

enum Suite:
  case Block(exprs: List[Expr])(val source: SourceRange)
  case Single(expr: Expr)(val source: SourceRange)

  def source: SourceRange

enum Type:
  case Name(value: String)(val source: SourceRange)
  case Paren(inner: Type)(val source: SourceRange)
  case App(base: Type, args: List[Type])(val source: SourceRange)

  def source: SourceRange

enum Expr:
  case Lit(value: Literal)(val source: SourceRange)
  case Var(name: String)(val source: SourceRange)
  case Paren(expr: Expr)(val source: SourceRange)
  case Block(exprs: List[Expr])(val source: SourceRange)
  // Call is used for both function calls, as well as expressions like "a + b"
  // For example, `a+b` is represented as `Call(Var("+"), List(Var("a"), Var("b")))`
  case Call(callee: Expr, typeArgs: List[Type], args: List[Expr], usingArgs: List[Expr])(val source: SourceRange)
  // Let and Var bindings
  case LetIn(name: String, isConstant: Boolean, tpe: Option[Type], value: Expr, body: Expr)(val source: SourceRange)
  case Bind(name: String, isConstant: Boolean, tpe: Option[Type], value: Expr)(val source: SourceRange)
  case Assign(name: String, value: Expr)(val source: SourceRange)
  case IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr)(val source: SourceRange)
  case For(name: String, inExpr: Expr, body: Suite)(val source: SourceRange)
  case While(cond: Expr, body: Suite)(val source: SourceRange)
  case Match(scrutinee: Expr, cases: List[MatchCase])(val source: SourceRange)
  case Return(expr: Expr)(val source: SourceRange)

  def source: SourceRange

final case class MatchCase(pattern: Pattern, body: Suite)(val source: SourceRange)

enum Literal:
  case IntLit(value: String)(val source: SourceRange)
  case BoolLit(value: Boolean)(val source: SourceRange)
  case StringLit(value: String)(val source: SourceRange)

  def source: SourceRange

enum Pattern:
  case Wildcard()(val source: SourceRange)
  case Lit(value: Literal)(val source: SourceRange)
  case BinderOrCtor0(name: String)(val source: SourceRange)
  case Ctor(name: String, args: List[Pattern])(val source: SourceRange)

  def source: SourceRange

package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange

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
  final case class InstanceSymbol(
      name: String,
      typeParams: List[String],
      headType: Type,
      givenParams: List[Type],
      members: Map[String, FunctionSymbol]
    ) extends TermSymbol:
    override def tpe: Type = headType
  final case class BuiltinValueSymbol(name: String, tpe: Type) extends TermSymbol
  final case class ErrorSymbol(name: String, tpe: Type) extends Symbol

  final case class FunctionSymbol(name: String, typeParams: List[String], tpe: Type.Fun, givenParams: List[Type]) extends Symbol
  final case class CtorSymbol(name: String, typeParams: List[String], tpe: Type.Fun, arity: Int, resultType: Type)
      extends Symbol
  case class Program(items: List[TopLevel])(val source: SourceRange)

  enum TopLevel:
    case DataDecl(name: String, typeParams: List[String], ctors: List[CtorDecl])(val source: SourceRange)
    case FunDecl(
        symbol: FunctionSymbol,
        typeParams: List[String],
        params: List[ParamSymbol],
        givenParams: List[ParamSymbol],
        body: Suite
      )(val source: SourceRange)
    case TypeClassDecl(
        name: String,
        typeParams: List[String],
        members: List[TypeClassMember]
      )(val source: SourceRange)
    case InstanceDecl(
        symbol: InstanceSymbol,
        typeParams: List[String],
        givenParams: List[ParamSymbol],
        members: List[InstanceMember]
      )(val source: SourceRange)

  final case class TypeClassMember(
      name: String,
      typeParams: List[String],
      params: List[Type],
      result: Type
    )(val source: SourceRange)

  final case class InstanceMember(
      memberName: String,
      symbol: FunctionSymbol,
      params: List[ParamSymbol],
      body: Suite
    )(val source: SourceRange)

  final case class CtorDecl(symbol: CtorSymbol, fields: List[CtorField])(val source: SourceRange)
  final case class CtorField(name: String, tpe: Type)(val source: SourceRange)

  enum Suite:
    case Block(exprs: List[Expr], tpe: Type)(val source: SourceRange)
    case Single(expr: Expr)(val source: SourceRange)

  sealed trait Expr:
    def tpe: Type
    def source: SourceRange

  object Expr:
    final case class Lit(value: ast.Literal, tpe: Type)(val source: SourceRange) extends Expr
    final case class Var(symbol: Symbol, tpe: Type)(val source: SourceRange) extends Expr
    final case class Paren(expr: Expr, tpe: Type)(val source: SourceRange) extends Expr
    final case class Block(exprs: List[Expr], tpe: Type)(val source: SourceRange) extends Expr
    final case class CallFun(callee: Expr, args: List[Expr], givenArgs: List[Expr], tpe: Type)(val source: SourceRange)
        extends Expr
    final case class CallCtor(symbol: CtorSymbol, args: List[Expr], tpe: Type)(val source: SourceRange) extends Expr
    final case class InstanceValue(symbol: InstanceSymbol, givenArgs: List[Expr], tpe: Type)(val source: SourceRange) extends Expr
    final case class CallTypeClassMember(instance: Expr, memberName: String, args: List[Expr], tpe: Type)(
        val source: SourceRange
      ) extends Expr
    final case class LetIn(
        symbol: LocalSymbol,
        isConstant: Boolean,
        declaredType: Option[Type],
        value: Expr,
        body: Expr,
        tpe: Type
      )(val source: SourceRange) extends Expr
    final case class Bind(
        symbol: LocalSymbol,
        isConstant: Boolean,
        declaredType: Option[Type],
        value: Expr,
        tpe: Type
      )(val source: SourceRange) extends Expr
    final case class Assign(symbol: LocalSymbol, value: Expr, tpe: Type)(val source: SourceRange) extends Expr
    final case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr, tpe: Type)(val source: SourceRange) extends Expr
    final case class For(symbol: LocalSymbol, inExpr: Expr, body: Suite, tpe: Type)(val source: SourceRange) extends Expr
    final case class While(cond: Expr, body: Suite, tpe: Type)(val source: SourceRange) extends Expr
    final case class Match(scrutinee: Expr, cases: List[MatchCase], tpe: Type)(val source: SourceRange) extends Expr
    final case class Return(expr: Expr, tpe: Type)(val source: SourceRange) extends Expr

  final case class MatchCase(pattern: Pattern, body: Suite)(val source: SourceRange)

  enum Pattern:
    case Wildcard()(val source: SourceRange)
    case Lit(value: ast.Literal)(val source: SourceRange)
    case Binder(symbol: LocalSymbol)(val source: SourceRange)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])(val source: SourceRange)

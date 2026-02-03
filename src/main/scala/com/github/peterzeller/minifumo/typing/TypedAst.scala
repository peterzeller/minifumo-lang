package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.typing.GlobalSymbols.PreEnv
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Sort, UnknownType}

import java.nio.file.Path

object TypedAst:
  sealed trait Symbol:
    def name: String
    def tpe: Expr

  sealed trait TermSymbol extends Symbol

  final case class LocalSymbol(name: String, tpe: Expr, id: Int) extends TermSymbol
  final case class ParamSymbol(name: String, tpe: Expr, id: Int) extends TermSymbol
  final case class BuiltinValueSymbol(name: String, tpe: Expr) extends TermSymbol
  final case class ErrorSymbol(name: String, tpe: Expr) extends Symbol
  final case class DatatypeSymbol(name: String, file: Path) extends Symbol:
    def tpe: Expr = Sort()(SourceRange.empty)

  final case class FunctionSymbol(name: String, tpe: Expr) extends Symbol
  final case class CtorSymbol(name: String, tpe: Expr) extends Symbol
  final case class GlobalNameSymbol(name: String, file: Path) extends Symbol:
    def tpe: Expr = UnknownType()(SourceRange.empty)

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
    case AppImplicit(callee: Expr, arg: Expr, tpe: Expr)(val source: SourceRange)
    case App(callee: Expr, arg: Expr, tpe: Expr)(val source: SourceRange)
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
    case UnknownType()(val source: SourceRange)

    def tpe(env: PreEnv): Expr =
      this match
        case Expr.Lit(value) =>
          value match
            case Literal.IntLit(value) =>
              val i = env.globalNames("Int")
              Expr.Var(DatatypeSymbol(i.name, i.file))(SourceRange.empty)
            case Literal.BoolLit(value) =>
              val i = env.globalNames("Bool")
              Expr.Var(DatatypeSymbol(i.name, i.file))(SourceRange.empty)
            case Literal.StringLit(value) =>
              val i = env.globalNames("String")
              Expr.Var(DatatypeSymbol(i.name, i.file))(SourceRange.empty)
            case Literal.UnitLit() =>
              val i = env.globalNames("Unit")
              Expr.Var(DatatypeSymbol(i.name, i.file))(SourceRange.empty)
        case Expr.Var(symbol) => symbol.tpe
        case Expr.AppImplicit(callee, arg, tpe) => tpe
        case Expr.App(callee, arg, tpe) => tpe
        case Expr.Pi(dom, cod) => Sort()(SourceRange.empty)
        case Expr.Sort() => Sort()(SourceRange.empty)
        case Expr.Lambda(param, body, tpe) => tpe
        case Expr.LetIn(symbol, isConstant, declaredType, value, body) => body.tpe(env)
        case Expr.Meta(index, tpe) => tpe
        case Expr.UnknownType() => Sort()(SourceRange.empty)


  class MatchCase(pattern: Pattern, body: Expr)(val source: SourceRange)

  enum Pattern:
    case Wildcard()(val source: SourceRange)
    case Lit(value: ast.Literal)(val source: SourceRange)
    case Binder(symbol: LocalSymbol)(val source: SourceRange)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])(val source: SourceRange)

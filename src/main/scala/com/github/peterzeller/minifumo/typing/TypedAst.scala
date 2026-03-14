package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Pi, Sort, UnknownType}
import com.github.peterzeller.minifumo.typing.UniverseLevel

sealed trait TypedAst:
  def source: SourceRange
  def children: List[TypedAst]

object TypedAst:
  case class Program(items: List[TopLevel])(val source: SourceRange) extends com.github.peterzeller.minifumo.typing.TypedAst:
    /** Returns top-level declarations in source order. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] = items

  enum TopLevel extends com.github.peterzeller.minifumo.typing.TypedAst:
    case DataDecl(symbol: DatatypeSymbol, typeParams: List[LocalSymbol], ctors: List[CtorDecl])(val source: SourceRange)
    case FunDecl(
        sig: FunSig,
        body: Expr
      )(val source: SourceRange)

    /** Returns child nodes for cursor-descent traversal. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] =
      this match
        case DataDecl(_, typeParams, ctors) =>
          typeParams.map(_.tpe) ++ ctors
        case FunDecl(sig, body) =>
          List(sig, body)

  case class FunSig(
    symbol: FunctionSymbol,
    typeParams: List[LocalSymbol],
    params: List[LocalSymbol],
    returnType: Expr,
  ) extends com.github.peterzeller.minifumo.typing.TypedAst:
    /** Returns the merged source range of all signature parts. */
    override def source: SourceRange =
      def earlier(left: ast.SourcePos, right: ast.SourcePos): ast.SourcePos =
        val lineCmp = left.line.compare(right.line)
        if lineCmp < 0 || (lineCmp == 0 && left.column <= right.column) then left else right
      val paramSources = (typeParams ++ params).map(_.tpe.source)
      val start = paramSources.foldLeft(returnType.source.start)((minStart, s) => earlier(s.start, minStart))
      SourceRange(start, returnType.source.end)

    /** Returns parameter types and return type as children. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] =
      typeParams.map(_.tpe) ++ params.map(_.tpe) :+ returnType

    /** Calculate the function type of the function signature */
    lazy val functionType: Expr =
      var r = returnType
      for p <- params.reverseIterator do
        r = Pi(p, r, false)(r.source)
      for p <- typeParams.reverseIterator do
        r = Pi(p, r, true)(r.source)
      r


  final case class CtorDecl(symbol: CtorSymbol, implicitFields: List[LocalSymbol], fields: List[LocalSymbol], returnType: Expr, tpe: Expr)(val source: SourceRange)
      extends com.github.peterzeller.minifumo.typing.TypedAst:
    /** Returns constructor field types, return type, and full constructor type. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] =
      implicitFields.map(_.tpe) ++ fields.map(_.tpe) ++ List(returnType, tpe)

  enum Expr extends com.github.peterzeller.minifumo.typing.TypedAst:
    def source: SourceRange

    case Lit(value: ast.Literal)(val source: SourceRange)
    case Var(symbol: Symbol)(val source: SourceRange)
    case AppImplicit(callee: Expr, arg: Expr, tpe: Expr)(val source: SourceRange)
    case App(callee: Expr, arg: Expr, tpe: Expr)(val source: SourceRange)
    case Pi(dom: LocalSymbol, cod: Expr, isImplicit: Boolean)(val source: SourceRange)
    case Sort(level: UniverseLevel = UniverseLevel.Type1)(val source: SourceRange)

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
    // axiom term accepted against any expected type
    case Axiom()(val source: SourceRange)
    case Match(scrutinee: Expr, motive: Expr, cases: List[MatchCase])(val source: SourceRange)

    /** Returns child nodes for cursor-descent traversal. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] =
      this match
        case Lit(_) | Var(_) | Sort(_) | UnknownType() | Axiom() =>
          Nil
        case AppImplicit(callee, arg, tpe) =>
          List(callee, arg, tpe)
        case App(callee, arg, tpe) =>
          List(callee, arg, tpe)
        case Pi(dom, cod, _) =>
          List(dom.tpe, cod)
        case Lambda(param, body, tpe) =>
          List(param.tpe, body, tpe)
        case LetIn(_, _, declaredType, value, body) =>
          List(declaredType, value, body)
        case Meta(_, tpe) =>
          List(tpe)
        case Match(scrutinee, motive, cases) =>
          List(scrutinee, motive) ++ cases

    // Some basic checks to catch some errors early
    this match {
      case i: App =>
        i.callee.calculateType match {
          case Some(p: TypedAst.Expr.Pi) =>
            require(!p.isImplicit)
          case _ =>
        }
      case _ =>
    }

    override def toString: String =
      TypeChecker.prettyExpr(this)

    def calculateType: Option[Expr] =
      this match {
        case Expr.Lit(value) =>
          None
        case Expr.Var(symbol) => Some(symbol.tpe)
        case Expr.AppImplicit(callee, arg, tpe) => Some(tpe)
        case Expr.App(callee, arg, tpe) => Some(tpe)
        case Expr.Pi(dom, cod, isImplicit) => Some(Expr.Sort(UniverseLevel.Type1)(SourceRange.empty))
        case Expr.Sort(level) => Some(Expr.Sort(UniverseLevel.Type1)(SourceRange.empty))
        case Expr.Lambda(param, body, tpe) => None
        case Expr.LetIn(symbol, isConstant, declaredType, value, body) => body.calculateType
        case Expr.Meta(index, tpe) => Some(tpe)
        case Expr.UnknownType() => Some(this)
        case Expr.Axiom() => None
        case Expr.Match(scrutinee, motive, cases) => None
      }

  final case class MatchCase(pattern: Pattern, body: Expr)(val source: SourceRange) extends com.github.peterzeller.minifumo.typing.TypedAst:
    /** Returns pattern and case body. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] = List(pattern, body)

  enum Pattern extends com.github.peterzeller.minifumo.typing.TypedAst:
    case Wildcard()(val source: SourceRange)
    case Lit(value: ast.Literal)(val source: SourceRange)
    case Binder(symbol: LocalSymbol)(val source: SourceRange)
    case Ctor(symbol: CtorSymbol, args: List[Pattern])(val source: SourceRange)

    /** Returns child nodes for cursor-descent traversal. */
    override def children: List[com.github.peterzeller.minifumo.typing.TypedAst] =
      this match
        case Wildcard() | Lit(_) | Binder(_) =>
          Nil
        case Ctor(_, args) =>
          args

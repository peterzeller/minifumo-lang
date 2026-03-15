package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.SourcePos
import com.github.peterzeller.minifumo.typing.TypedAst.Expr

import scala.collection.mutable

/** Resolves hover information (type + declaration comment) for typed programs. */
object HoverLookup:
  final case class HoverInfo(typeText: String, comment: Option[String])

  /** Resolves hover information for the symbol under the given source position. */
  def hoverAt(program: TypedAst.Program, pos: SourcePos, currentFile: String): Option[HoverInfo] =
    val localComments = collectLocalComments(program)
    resolveAt(program, pos, currentFile, localComments)
      .orElse {
        if pos.column > 1 then resolveAt(program, SourcePos(pos.line, pos.column - 1), currentFile, localComments)
        else None
      }

  /** Resolves hover information from one cursor position. */
  private def resolveAt(program: TypedAst.Program, pos: SourcePos, currentFile: String, localComments: Map[Int, String]): Option[HoverInfo] =
    DefinitionLookup.findElementAtCursor(program, pos).flatMap {
      case Expr.Var(symbol) => hoverForSymbol(symbol, localComments)
      case Expr.App(Expr.Var(symbol), _, _) => hoverForSymbol(symbol, localComments)
      case Expr.AppImplicit(Expr.Var(symbol), _, _) => hoverForSymbol(symbol, localComments)
      case TypedAst.Pattern.Ctor(symbol, _) => hoverForSymbol(symbol, localComments)
      case _ => None
    }

  /** Builds hover information for one resolved symbol. */
  private def hoverForSymbol(symbol: Symbol, localComments: Map[Int, String]): Option[HoverInfo] =
    val typeText = TypeChecker.prettyExpr(symbol.tpe)
    val comment = symbol match
      case functionSymbol: FunctionSymbol => functionSymbol.continuationData.flatMap(_.declAst.comment.map(_.text))
      case datatypeSymbol: DatatypeSymbol => datatypeSymbol.continuationData.flatMap(_.declAst.comment.map(_.text))
      case ctorSymbol: CtorSymbol =>
        ctorSymbol.dt.continuationData.flatMap(_.declAst.ctors.find(_.name == ctorSymbol.name).flatMap(_.comment.map(_.text)))
      case localSymbol: LocalSymbol => localComments.get(localSymbol.id)
      case _ => None
    Some(HoverInfo(typeText, comment))

  /** Collects local symbol comments introduced by let binders. */
  private def collectLocalComments(program: TypedAst.Program): Map[Int, String] =
    val comments = mutable.Map[Int, String]()
    for item <- program.items do
      collectItemComments(item, comments)
    comments.toMap

  /** Collects local comments from a top-level item. */
  private def collectItemComments(item: TypedAst.TopLevel, comments: mutable.Map[Int, String]): Unit =
    item match
      case TypedAst.TopLevel.DataDecl(_, _, ctors) =>
        for ctor <- ctors do collectExprComments(ctor.returnType, comments)
      case TypedAst.TopLevel.FunDecl(sig, body) =>
        collectExprComments(sig.returnType, comments)
        collectExprComments(body, comments)

  /** Collects let-binder comments from an expression tree. */
  private def collectExprComments(expr: Expr, comments: mutable.Map[Int, String]): Unit =
    expr match
      case letExpr@Expr.LetIn(symbol, _, declaredType, value, body) =>
        letExpr.comment.foreach(c => comments.getOrElseUpdate(symbol.id, c.text))
        collectExprComments(declaredType, comments)
        collectExprComments(value, comments)
        collectExprComments(body, comments)
      case _ =>
        expr.children.collect { case child: Expr => child }.foreach(collectExprComments(_, comments))

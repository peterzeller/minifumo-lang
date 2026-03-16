package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.SourcePos
import com.github.peterzeller.minifumo.typing.TypedAst.Expr


/** Resolves hover information (type + declaration comment) for typed programs. */
object HoverLookup:
  final case class HoverInfo(typeText: String, comment: Option[String])

  /** Resolves hover information for the symbol under the given source position. */
  def hoverAt(program: TypedAst.Program, pos: SourcePos, currentFile: String): Option[HoverInfo] =
    resolveAt(program, pos, currentFile)
      .orElse {
        if pos.column > 1 then resolveAt(program, SourcePos(pos.line, pos.column - 1), currentFile)
        else None
      }

  /** Resolves hover information from one cursor position. */
  private def resolveAt(program: TypedAst.Program, pos: SourcePos, currentFile: String): Option[HoverInfo] =
    DefinitionLookup.findElementAtCursor(program, pos).flatMap {
      case Expr.Var(symbol) => Some(hoverForSymbol(symbol))
      case Expr.App(Expr.Var(symbol), _, _) => Some(hoverForSymbol(symbol))
      case Expr.AppImplicit(Expr.Var(symbol), _, _) => Some(hoverForSymbol(symbol))
      case TypedAst.Pattern.Ctor(symbol, _) => Some(hoverForSymbol(symbol))
      case other => Some(HoverInfo("", Some(s"Hovering over $other")))
    }

  /** Builds hover information for one resolved symbol. */
  private def hoverForSymbol(symbol: Symbol): HoverInfo =
    val typeText = symbol.name + ": " + TypeChecker.prettyExpr(symbol.tpe)
    val comment: Option[String] = symbol match
      case functionSymbol: FunctionSymbol => functionSymbol.continuationData.map(_.declAst.comment)
      case datatypeSymbol: DatatypeSymbol => datatypeSymbol.continuationData.map(_.declAst.comment)
      case ctorSymbol: CtorSymbol =>
        ctorSymbol.dt.continuationData.flatMap(_.declAst.ctors.find(_.name == ctorSymbol.name).map(_.comment))
      case localSymbol: LocalSymbol => Some(localSymbol.comment)
      case _ => None
    HoverInfo(typeText, comment)


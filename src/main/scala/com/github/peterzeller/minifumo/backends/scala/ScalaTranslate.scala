package com.github.peterzeller.minifumo.backends.scala

import com.github.peterzeller.minifumo.ast.Literal
import com.github.peterzeller.minifumo.typing.{ErrorSymbol, GlobalSymbol, LocalSymbol, Symbol, TermSymbol, TypedAst}
import com.github.peterzeller.minifumo.typing.TypedAst.TopLevel.{DataDecl, FunDecl}
import com.github.peterzeller.minifumo.typing.TypedAst.{Expr, Pattern, Program, TopLevel}

object ScalaTranslate:
  def translateProg(prog: Program): String =
    prog.items.map(translateToplevel).mkString("\n\n")

  private def translateToplevel(tld: TopLevel): String =
    tld match {
      case f: TopLevel.DataDecl =>
        translateDataDecl(f)
      case f: TopLevel.FunDecl =>
        translateFunDecl(f)
    }

  private def translateTypeParams(typeParams: List[LocalSymbol]): String =
      if typeParams.isEmpty then ""
      else {
        def t(l: LocalSymbol) =
          l.tpe match {
            case Expr.Sort(level) => ""
            case other => s": ${translateType(other)}"
          }
        s"[${typeParams.map(l => s"${l.name}${t(l)}").mkString(", ")}]"
      }

  private def translateDataDecl(d: DataDecl): String = {

    s"""
      |enum ${d.symbol.name}${translateTypeParams(d.typeParams)}:
      |${d.ctors.map(translateConstructor).mkString("\n")}
      |""".stripMargin
  }

  private def translateConstructor(c: TypedAst.CtorDecl): String =
    s"    case ${c.symbol.name}(${translateParams(c.fields)})"


  private def translateParams(params: List[LocalSymbol]): String =
    params.map(translateField).mkString(", ")

  private def translateField(l: LocalSymbol): String =
    s"${l.name}: ${translateType(l.tpe)}"

  private def translateType(t: TypedAst.Expr): String =
    t match {
      case Expr.Var(symbol) => fullyQualified(symbol)
      case Expr.AppImplicit(callee, arg, tpe) =>
        s"${translateType(callee)}[${translateType(arg)}]"
      case Expr.App(callee, arg, tpe) =>
        s"${translateType(callee)}[${translateType(arg)}]"
      case Expr.Pi(dom, cod, isImplicit) =>
        s"(${translateField(dom)}) => ${translateType(cod)}"
      case _ => "Any"
    }

  private def fullyQualified(s: Symbol): String =
    s match {
      case symbol: TermSymbol =>
        symbol.name
      case symbol: GlobalSymbol =>
        symbol.file.replace(".minifumo", "").replace("/", ".") + "." + symbol.name
      case ErrorSymbol(name, tpe) =>
        name
    }

  private def translateFunDecl(f: FunDecl): String = {
    s"def ${f.sig.symbol.name}${translateTypeParams(f.sig.typeParams)}(${translateParams(f.sig.params)}): ${translateType(f.sig.returnType)} =\n    ${printExpr(f.body, 4)}"
  }

  private def printExpr(e: Expr, indent: Int): String =
    e match {
      case Expr.Lit(value) => translateLit(value)
      case Expr.Var(symbol) => fullyQualified(symbol)
      case Expr.AppImplicit(callee, arg, tpe) =>
        s"${printExpr(callee, indent)}[${printExpr(arg, indent)}]"
      case Expr.App(callee, arg, tpe) =>
        s"${printExpr(callee, indent)}(${printExpr(arg, indent)})"
      case Expr.Pi(dom, cod, isImplicit) =>
        s"(${translateField(dom)}) => ${translateType(cod)}"
      case Expr.Sort(level) =>
        s"Any"
      case Expr.Lambda(param, body, tpe) =>
        s"(${translateField(param)}) => ${printExpr(body, indent)}"
      case Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        s"val ${translateField(symbol)} = ${printExpr(value, indent)}\n${printIndent(indent)}${printExpr(body, indent)}"
      case Expr.Meta(index, tpe) => "???"
      case Expr.UnknownType() => "???"
      case Expr.Axiom() => "???"
      case Expr.Match(scrutinee, motive, cases) =>
        s"${printExpr(scrutinee, indent)} match${cases.map(printCase(_, indent+4)).mkString("")}"
    }

  private def printCase(c: TypedAst.MatchCase, indent: Int): String =
    s"\n${printIndent(indent)}case ${printPattern(c.pattern)} =>\n${printIndent(indent+4)}${printExpr(c.body, indent+4)}"

  private def printIndent(indent: Int): String = " " * indent

  private def printPattern(p: Pattern): String =
    p match {
      case Pattern.Wildcard() => "_"
      case p: Pattern.Lit => translateLit(p.value)
      case Pattern.Binder(symbol) => symbol.name
      case Pattern.Ctor(symbol, args) if args.isEmpty => symbol.name
      case Pattern.Ctor(symbol, args) =>
        s"${symbol.name}(${args.map(printPattern).mkString(", ")})"
    }

  private def translateLit(literal: Literal): String =
    literal match {
      case Literal.IntLit(value: String) => value
      case Literal.BoolLit(value: Boolean) => value.toString
      case Literal.StringLit(value: String) => s"\"$value\""
      case Literal.UnitLit() => "()"
    }
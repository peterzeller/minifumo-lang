package com.github.peterzeller.minifumo.ast

import scala.math.Ordering.Implicits.*

case class SourcePos(line: Int, column: Int)

object SourcePos:
  // Orders positions by line, then column.
  given ordering: Ordering[SourcePos] with
    def compare(x: SourcePos, y: SourcePos): Int =
      val lineCmp = x.line.compare(y.line)
      if lineCmp != 0 then lineCmp else x.column.compare(y.column)





case class SourceRange(start: SourcePos, end: SourcePos):
  // Checks whether a position lies within the range (inclusive).
  def contains(pos: SourcePos): Boolean =
    start <= pos && pos <= end

  // Merges two ranges by taking the earliest start and latest end.
  def merge(other: SourceRange): SourceRange =
    SourceRange(
     start min other.start,
     end max other.end
    )

object SourceRange:
  def empty: SourceRange = SourceRange(SourcePos(0,0),SourcePos(0,0))

case class SourceRangeWithFile(file: String, range: SourceRange)



case class ProgramFile(imports: List[ImportStatement], items: List[TopLevel])(val source: SourceRange)

final case class ImportStatement(name: String, from: Option[String], inRepo: Option[String])(val source: SourceRange)

// Top-level declarations in a file
enum TopLevel:
  // defines a data type with constructors
  case DataDecl(
    name: String,
    implicitParams: List[FunParam],
    params: List[FunParam],
    ctors: List[CtorDecl],
    exported: Boolean,
    isProp: Boolean
  )(val source: SourceRange)

  // a function definition with implementation
  case FunDecl(
    sig: FunSig,
    body: Expr,
    exported: Boolean
  )(val source: SourceRange)

  def source: SourceRange

// A function signature
final case class FunSig(
    // name of the function
    name: String,
    // implicit parameters in square brackets, e.g. `[T]`
    implicitParams: List[FunParam],
    // regular parameters in parentheses
    params: List[FunParam],
    // optional return type annotation
    returnType: Expr
  )(val source: SourceRange)

final case class CtorDecl(name: String, fields: List[CtorField], returnType: Option[Expr])(val source: SourceRange)
final case class CtorField(name: String, tpe: Expr)(val source: SourceRange)

final case class FunParam(name: String, tpe: Expr)(val source: SourceRange)
final case class LambdaParam(name: String, tpe: Option[Expr])(val source: SourceRange)
final case class PiParam(name: String, tpe: Expr)(val source: SourceRange)


enum Expr:
  case Lit(value: Literal)(val source: SourceRange)
  case Var(name: String)(val source: SourceRange)
  // a function call where an implicit argument is passed in square brackets.
  // For example id[T](x) is represented as Call(CallImplicit(Var("id"), Var("T")), Var("x"))
  // Calls with multiple implicit arguments are nested CallImplicit nodes (curried).
  case CallImplicit(callee: Expr, arg: Expr)(val source: SourceRange)
  // A function call with explicit argument.
  // Calls with multiple arguments are nested Call nodes (curried).
  // For example, f(x, y) is represented as Call(Call(Var("f"), Var("x")), Var("y"))
  case Call(callee: Expr, arg: Expr)(val source: SourceRange)
  // Field access syntax, for example p.left.
  case FieldAccess(receiver: Expr, fieldName: String)(val source: SourceRange)
  // Anonymous function (lambda expression), for example (x: Int) => x + 1
  case Lambda(param: LambdaParam, body: Expr)(val source: SourceRange)
  // Let expressions, for example let x: Int = 5 in x + 1

  // Dependent function types (Pi types)
  case Pi(param: PiParam, body: Expr)(val source: SourceRange)

  case LetIn(name: String, tpe: Option[Expr], value: Expr, body: Expr)(val source: SourceRange)
  case Match(scrutinee: Expr, factName: Option[String], cases: List[MatchCase])(val source: SourceRange)

  // place holder for missing expressions
  case Hole()(val source: SourceRange)
  // axiom placeholder inhabiting any expected type during checking
  case Axiom()(val source: SourceRange)

  def source: SourceRange

  override def toString: String =
    this match {
      case Expr.Lit(value) =>
        value match {
          case Literal.IntLit(value) => value.toString
          case Literal.BoolLit(value) => value.toString
          case Literal.StringLit(value) => s"\"$value\""
          case Literal.UnitLit() => "unit"
        }
      case Expr.Var(name) =>
        name
      case Expr.CallImplicit(callee, arg) =>
        s"$callee[$arg]"
      case Expr.Call(callee, arg) =>
        s"$callee($arg)"
      case Expr.FieldAccess(receiver, fieldName) =>
        s"$receiver.$fieldName"
      case Expr.Lambda(param, body) =>
        s"(fun $param => $body)"
      case Expr.Pi(param, body) =>
        s"($param => $body)"
      case Expr.LetIn(name, tpe, value, body) =>
        s"let $name: $tpe = $value in $body"
      case Expr.Match(scrutinee, factName, cases) =>
        val factPrefix = factName.map(name => s"$name: ").getOrElse("")
        s"match $factPrefix$scrutinee ${cases.map(c => s"$c").mkString(" | ")}"
      case Expr.Hole() => "???"
      case Expr.Axiom() => "axiom"
    }

final case class MatchCase(pattern: Pattern, body: Expr)(val source: SourceRange)

enum Literal:
  case IntLit(value: String)(val source: SourceRange)
  case BoolLit(value: Boolean)(val source: SourceRange)
  case StringLit(value: String)(val source: SourceRange)
  case UnitLit()(val source: SourceRange)

  def source: SourceRange

enum Pattern:
  case Wildcard()(val source: SourceRange)
  case Lit(value: Literal)(val source: SourceRange)
  case BinderOrCtor0(name: String)(val source: SourceRange)
  case Ctor(name: String, args: List[Pattern])(val source: SourceRange)

  def source: SourceRange

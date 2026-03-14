package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.typing.TypedAst.{Expr, Program, TopLevel}

/** Removes datatype index parameters from typed datatype declarations and their uses. */
object DatatypeIndexErasure:

  /** Stores which datatype parameter positions should be kept for a datatype symbol. */
  private final case class DatatypeParamMask(keepFlags: Vector[Boolean])

  /** Represents one application argument together with whether it is implicit. */
  private final case class AppArg(value: Expr, isImplicit: Boolean, appType: Expr)

  /** Returns a copy of the program where datatypes keep only true type parameters and matching arguments. */
  def erase(program: Program): Program =
    val datatypeMasks = collectDatatypeParamMasks(program)
    TypedAstRewriter.rewrite(program) {
      case dataDecl @ TopLevel.DataDecl(symbol, typeParams, ctors) =>
        val filteredParams = filterDatatypeParams(symbol, typeParams, datatypeMasks)
        TopLevel.DataDecl(symbol, filteredParams, ctors)(dataDecl.source)
      case expr: Expr =>
        rewriteDatatypeApplication(expr, datatypeMasks)
    }

  /** Collects per-datatype keep/drop masks from original datatype parameter declarations. */
  private def collectDatatypeParamMasks(program: Program): Map[DatatypeSymbol, DatatypeParamMask] =
    program.items.collect {
      case TopLevel.DataDecl(symbol, typeParams, _) =>
        symbol -> DatatypeParamMask(typeParams.map(param => isTypeParameter(param.tpe)).toVector)
    }.toMap

  /** Filters datatype parameters using the keep/drop mask for this datatype symbol. */
  private def filterDatatypeParams(
      symbol: DatatypeSymbol,
      typeParams: List[LocalSymbol],
      datatypeMasks: Map[DatatypeSymbol, DatatypeParamMask]
    ): List[LocalSymbol] =
    datatypeMasks.get(symbol) match
      case Some(mask) =>
        typeParams.zip(mask.keepFlags).collect { case (param, true) => param }
      case None =>
        typeParams

  /** Rewrites one datatype application expression by dropping arguments of erased index parameters. */
  private def rewriteDatatypeApplication(expr: Expr, datatypeMasks: Map[DatatypeSymbol, DatatypeParamMask]): Expr =
    extractDatatypeApplication(expr) match
      case Some((symbol, head, args)) =>
        datatypeMasks.get(symbol) match
          case Some(mask) =>
            val keptArgs = args.zipWithIndex.collect {
              case (arg, index) if index >= mask.keepFlags.length || mask.keepFlags(index) => arg
            }
            rebuildApplication(head, keptArgs, expr.source)
          case None =>
            expr
      case None =>
        expr

  /** Extracts the application head and argument spine when the expression applies a datatype symbol. */
  private def extractDatatypeApplication(expr: Expr): Option[(DatatypeSymbol, Expr, List[AppArg])] =
    val (head, args) = flattenApplication(expr)
    head match
      case Expr.Var(symbol: DatatypeSymbol) => Some((symbol, head, args))
      case _ => None

  /** Flattens a nested application into a head expression and ordered arguments. */
  private def flattenApplication(expr: Expr): (Expr, List[AppArg]) =
    def loop(current: Expr, reversedArgs: List[AppArg]): (Expr, List[AppArg]) =
      current match
        case Expr.App(callee, arg, tpe) =>
          loop(callee, AppArg(arg, isImplicit = false, tpe) :: reversedArgs)
        case Expr.AppImplicit(callee, arg, tpe) =>
          loop(callee, AppArg(arg, isImplicit = true, tpe) :: reversedArgs)
        case _ =>
          (current, reversedArgs)
    loop(expr, Nil)

  /** Rebuilds a nested application expression from a head and ordered arguments. */
  private def rebuildApplication(head: Expr, args: List[AppArg], source: com.github.peterzeller.minifumo.ast.SourceRange): Expr =
    args.foldLeft(head) { (callee, arg) =>
      if arg.isImplicit then Expr.AppImplicit(callee, arg.value, arg.appType)(source)
      else Expr.App(callee, arg.value, arg.appType)(source)
    }

  /** Returns true when a datatype parameter has a type-sort annotation like `Type` or `Prop`. */
  private def isTypeParameter(tpe: Expr): Boolean =
    tpe match
      case Expr.Sort(_) => true
      case _ => false

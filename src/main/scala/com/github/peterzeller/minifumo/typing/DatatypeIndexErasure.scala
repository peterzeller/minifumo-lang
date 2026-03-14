package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.typing.TypedAst.{Program, TopLevel}

/** Removes datatype index parameters from typed datatype declarations. */
object DatatypeIndexErasure:

  /** Returns a copy of the program where datatypes keep only true type parameters. */
  def erase(program: Program): Program =
    Program(program.items.map(eraseTopLevel))(program.source)

  /** Rewrites one top-level declaration and trims non-type datatype parameters. */
  private def eraseTopLevel(topLevel: TopLevel): TopLevel =
    topLevel match
      case TopLevel.DataDecl(symbol, typeParams, ctors) =>
        val keptTypeParams = typeParams.filter(param => isTypeParameter(param.tpe))
        TopLevel.DataDecl(symbol, keptTypeParams, ctors)(topLevel.source)
      case _ =>
        topLevel

  /** Returns true when a datatype parameter has a type-sort annotation like `Type` or `Prop`. */
  private def isTypeParameter(tpe: TypedAst.Expr): Boolean =
    tpe match
      case TypedAst.Expr.Sort(_) => true
      case _ => false

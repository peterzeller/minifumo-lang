package com.github.peterzeller.minifumo.interpreter

import com.github.peterzeller.minifumo.interpreter.Interpreter.Value
import com.github.peterzeller.minifumo.interpreter.Interpreter.Value.{AdtVal, FuncVal}

object BuiltInFunctions:

  val overrides: Map[String,Value.FuncVal] = List(
    printlnString
  ).map(e => e.name -> e).toMap

  private def printlnString: FuncVal =
    FuncVal("printlnString", s => {
        println(s"${convertString(s)}")
        AdtVal("MakeUnit", List())
    })

  private def convertString(v: Value): String =
    v match
      case Value.AdtVal("MakeString", List(s)) =>
        convertList(s, convertChar).mkString("")
      case _ =>
        s"invalid string $v"

  private def convertList[T](v: Value, convertItem: Value => T): List[T] =
    v match
      case Value.AdtVal("Cons", List(x, xs)) =>
        convertItem(x) :: convertList(xs, convertItem)
      case AdtVal("Nil", List()) =>
        Nil
      case _ =>
        throw new RuntimeException(s"not a list: $v")

  private def convertChar(v: Value): Char =
    v match
      case Value.AdtVal("MakeChar", List(n)) =>
        convertNat(n).toChar
      case _ =>
        throw new RuntimeException(s"not a char: $v")

  private def convertNat(v: Value): BigInt =
    v match
      case AdtVal("Suc", List(x)) =>
        1 + convertNat(x)
      case AdtVal("Zero", List()) =>
        0
      case _ =>
        throw new RuntimeException(s"not a nat: $v")
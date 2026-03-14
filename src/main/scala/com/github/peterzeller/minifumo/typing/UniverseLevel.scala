package com.github.peterzeller.minifumo.typing

/** Represents sort levels used by `Sort(level)` expressions. */
enum UniverseLevel:
  /** A concrete non-negative universe level. */
  case Level(value: Int)
  /** A named generic universe variable such as `u` or `v`. */
  case Generic(name: String)
  /** The maximum of two universe levels. */
  case Max(left: UniverseLevel, right: UniverseLevel)

object UniverseLevel:
  /** The proposition universe `Sort(0)`. */
  val Prop: UniverseLevel = Level(0)
  /** Default fallback for non-inferred `Type`, i.e. `Sort(1)`. */
  val Type1: UniverseLevel = Level(1)

  /** Renders a universe level in source-like syntax. */
  def pretty(level: UniverseLevel): String =
    level match
      case Level(value) => value.toString
      case Generic(name) => name
      case Max(left, right) => s"max(${pretty(left)}, ${pretty(right)})"

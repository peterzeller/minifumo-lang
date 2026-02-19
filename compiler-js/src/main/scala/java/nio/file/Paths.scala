package java.nio.file

import scala.annotation.static

/** Minimal Scala.js-compatible Paths helper for constructing Path values. */
final class Paths

object Paths:
  // Creates a path from one raw string and optional extra segments.
  @static def get(first: String, more: String*): Path =
    Path.of(first, more*)

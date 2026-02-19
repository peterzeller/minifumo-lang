package java.nio.file

import java.io.File
import scala.annotation.static

/** Minimal Scala.js-compatible Path implementation used by the browser build. */
final class Path(val raw: String):
  // Resolves a child segment against this path.
  def resolve(other: String): Path =
    val separator = if raw.endsWith("/") then "" else "/"
    Path.of(s"$raw$separator$other")

  // Returns the parent path when available.
  def getParent: Path =
    val normalized = raw.stripSuffix("/")
    val index = normalized.lastIndexOf('/')
    if index <= 0 then this
    else Path.of(normalized.substring(0, index))

  // Computes a relative path by trimming this path prefix from another path.
  def relativize(other: Path): Path =
    val base = raw.stripSuffix("/")
    val target = other.toString
    if target.startsWith(base + "/") then Path.of(target.drop(base.length + 1)) else other

  // Returns a normalized representation for this simplified path.
  def normalize(): Path =
    this

  // Checks whether the path looks like an absolute filesystem path.
  def isAbsolute: Boolean =
    raw.startsWith("/") || raw.matches("^[A-Za-z]:\\\\.*")

  // Checks whether this path string ends with another path string.
  def endsWith(other: String): Boolean =
    raw.endsWith(other)

  // Converts this path to a java.io.File placeholder.
  def toFile: File =
    new File(raw)

  // Returns the underlying path text.
  override def toString: String =
    raw

object Path:
  // Builds a new path from a raw string and optional extra segments.
  @static def of(first: String, more: String*): Path =
    val all = first :: more.toList
    new Path(all.mkString("/"))

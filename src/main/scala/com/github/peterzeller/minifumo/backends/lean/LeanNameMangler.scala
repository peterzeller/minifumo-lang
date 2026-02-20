package com.github.peterzeller.minifumo.backends.lean

import scala.collection.mutable

object LeanNameMangler:
  // Represents the namespace in which a symbol name must be unique.
  enum NameKind:
    case ModuleName
    case GlobalName
    case LocalName

  private val reservedWords = Set(
    "import", "namespace", "open", "def", "theorem", "axiom", "inductive", "structure", "where",
    "match", "with", "let", "in", "if", "then", "else", "termination_by", "decreasing_by", "by",
    "Type", "Prop", "Sort", "Nat", "Int", "Bool", "String", "Unit", "List", "Option", "Prod"
  )

  // Mangling context for deterministic collision-free name assignment.
  final class Context:
    private val used = mutable.Set[String]()
    private val mapping = mutable.Map[(NameKind, String), String]()

    // Returns a stable Lean-safe symbol name.
    def mangle(kind: NameKind, raw: String): String =
      mapping.getOrElseUpdate((kind, raw), {
        val base = sanitize(raw, kind)
        var candidate = base
        var i = 1
        while used.contains(candidate) || reservedWords.contains(candidate) do
          candidate = s"${base}_${i}"
          i += 1
        used += candidate
        candidate
      })

  // Produces a Lean module name from a relative Minifumo file path.
  def moduleNameForPath(relativePath: String): String =
    val withoutSuffix = if relativePath.endsWith(".minifumo") then relativePath.stripSuffix(".minifumo") else relativePath
    withoutSuffix
      .split("[/\\\\.]+")
      .filter(_.nonEmpty)
      .map(segment => sanitize(segment, NameKind.ModuleName))
      .mkString("_")

  // Converts a raw Minifumo identifier to a Lean-safe base token.
  private def sanitize(raw: String, kind: NameKind): String =
    val normalized = raw.map:
      case ch if ch.isLetterOrDigit || ch == '_' => ch
      case _ => '_'
    val prefixed =
      if normalized.isEmpty then
        "minifumo"
      else if normalized.head.isDigit then
        s"n_${normalized}"
      else
        normalized
    kind match
      case NameKind.LocalName => s"l_${prefixed}"
      case NameKind.GlobalName => s"g_${prefixed}"
      case NameKind.ModuleName => s"M_${prefixed}"


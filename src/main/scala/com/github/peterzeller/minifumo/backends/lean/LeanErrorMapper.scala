package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.ast.{SourcePos, SourceRange}
import com.github.peterzeller.minifumo.backends.lean.LeanBackend.{BackendError, GeneratedLeanFile}
import com.github.peterzeller.minifumo.backends.lean.LeanRunner.LeanDiagnostic
import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath

import java.nio.file.Path

object LeanErrorMapper:
  // Maps Lean diagnostics in generated files back to original Minifumo file locations.
  def mapLeanErrors(diagnostics: List[LeanDiagnostic], generatedFiles: List[GeneratedLeanFile]): List[MinifumoErrorWithPath] =
    val generatedByPath = generatedFiles.map(file => file.path.toAbsolutePath.normalize() -> file).toMap
    diagnostics.map: diag =>
      val diagPath = Path.of(diag.file).toAbsolutePath.normalize()
      generatedByPath.get(diagPath) match
        case Some(generated) =>
          generated.lineMap.find(entry => diag.line >= entry.startLine && diag.line <= entry.endLine) match
            case Some(entry) =>
              MinifumoErrorWithPath(entry.sourcePath, BackendError(s"Lean ${diag.message}", entry.sourceRange))
            case None =>
              val fallbackRange = SourceRange(SourcePos(1, 1), SourcePos(1, 1))
              MinifumoErrorWithPath(Path.of(diag.file), BackendError(s"Lean ${diag.message}", fallbackRange))
        case None =>
          val fallbackRange = SourceRange(SourcePos(1, 1), SourcePos(1, 1))
          MinifumoErrorWithPath(Path.of(diag.file), BackendError(s"Lean ${diag.message}", fallbackRange))


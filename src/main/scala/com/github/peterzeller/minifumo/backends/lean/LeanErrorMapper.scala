package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.ast.{SourcePos, SourceRange}
import com.github.peterzeller.minifumo.backends.lean.LeanBackend.{BackendError, GeneratedLeanFile}
import com.github.peterzeller.minifumo.backends.lean.LeanRunner.LeanDiagnostic
import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath

import java.nio.file.{Path}

object LeanErrorMapper:
  // Formats one Lean diagnostic with generated-file location details.
  private def leanMessage(diag: LeanDiagnostic, generatedPath: Path): String =
    s"Lean ${generatedPath}:${diag.line}:${diag.column} ${diag.message}"

  // Maps Lean diagnostics in generated files back to original Minifumo file locations.
  def mapLeanErrors(diagnostics: List[LeanDiagnostic], generatedFiles: List[GeneratedLeanFile]): List[MinifumoErrorWithPath] =
    val generatedByPath = generatedFiles.map(file => file.path.toAbsolutePath.normalize() -> file).toMap

    // Resolves a lean-reported path to the generated output path.
    def resolveDiagnosticPath(diag: LeanDiagnostic): Path =
      val raw = Path.of(diag.file)
      if raw.isAbsolute then raw.toAbsolutePath.normalize()
      else
        generatedFiles.find(_.path.getFileName.toString == diag.file) match
          case Some(g) => g.path.toAbsolutePath.normalize()
          case None => raw.toAbsolutePath.normalize()

    diagnostics.map: diag =>
      val diagPath = resolveDiagnosticPath(diag)
      generatedByPath.get(diagPath) match
        case Some(generated) =>
          generated.lineMap.find(entry => diag.line >= entry.startLine && diag.line <= entry.endLine) match
            case Some(entry) =>
              MinifumoErrorWithPath(entry.sourcePath.toString, BackendError(leanMessage(diag, generated.path), entry.sourceRange))
            case None =>
              val fallbackRange = SourceRange(SourcePos(1, 1), SourcePos(1, 1))
              MinifumoErrorWithPath(diagPath.toString, BackendError(leanMessage(diag, generated.path), fallbackRange))
        case None =>
          val fallbackRange = SourceRange(SourcePos(1, 1), SourcePos(1, 1))
          MinifumoErrorWithPath(diagPath.toString, BackendError(leanMessage(diag, diagPath), fallbackRange))

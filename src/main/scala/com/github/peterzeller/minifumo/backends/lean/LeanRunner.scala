package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.backends.lean.LeanBackend.GeneratedLeanFile

import scala.sys.process.Process

object LeanRunner:
  // Represents one Lean diagnostic produced by running the Lean compiler.
  final case class LeanDiagnostic(file: String, line: Int, column: Int, level: String, message: String)

  // Runs Lean on each generated file and collects all diagnostics.
  def checkGeneratedFiles(files: List[GeneratedLeanFile]): List[LeanDiagnostic] =
    files.flatMap: generated =>
      val workDir = Option(generated.path.toAbsolutePath.normalize().getParent).map(_.toFile).orNull
      val leanFile = generated.path.getFileName.toString
      val oleanFile = leanFile.stripSuffix(".lean") + ".olean"
      val cmd = Seq("lean", "-o", oleanFile, leanFile)
      val output = new StringBuilder
      val logger = scala.sys.process.ProcessLogger(
        line => { output.append(line).append("\n"); () },
        line => { output.append(line).append("\n"); () }
      )
      val env = if workDir == null then Seq.empty else Seq("LEAN_PATH" -> workDir.getAbsolutePath)
      val code = Process(cmd, workDir, env*).!(logger)
      if code == 0 then Nil else parseDiagnostics(output.toString).filter(_.level == "error")

  // Parses Lean stdout/stderr text into structured diagnostics.
  private def parseDiagnostics(output: String): List[LeanDiagnostic] =
    val regex = "^(.+):(\\d+):(\\d+):\\s*(error|warning):\\s*(.*)$".r
    output.linesIterator.flatMap {
      case regex(file, line, col, level, msg) =>
        Some(LeanDiagnostic(file, line.toInt, col.toInt, level, s"${level}: ${msg}"))
      case _ => None
    }.toList


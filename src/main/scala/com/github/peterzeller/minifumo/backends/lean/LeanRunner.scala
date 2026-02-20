package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.backends.lean.LeanBackend.GeneratedLeanFile

import scala.sys.process.Process

object LeanRunner:
  // Represents one Lean diagnostic produced by running the Lean compiler.
  final case class LeanDiagnostic(file: String, line: Int, column: Int, message: String)

  // Runs Lean on each generated file and collects all diagnostics.
  def checkGeneratedFiles(files: List[GeneratedLeanFile]): List[LeanDiagnostic] =
    files.flatMap: generated =>
      val cmd = Seq("lean", generated.path.toString)
      val output = new StringBuilder
      val logger = scala.sys.process.ProcessLogger(line => { output.append(line).append("\n"); () }, line => { output.append(line).append("\n"); () })
      val code = Process(cmd).!(logger)
      if code == 0 then Nil else parseDiagnostics(output.toString)

  // Parses Lean stdout/stderr text into structured diagnostics.
  private def parseDiagnostics(output: String): List[LeanDiagnostic] =
    val regex = "^(.+):(\\d+):(\\d+):\\s*(error|warning):\\s*(.*)$".r
    output.linesIterator.flatMap {
      case regex(file, line, col, level, msg) =>
        Some(LeanDiagnostic(file, line.toInt, col.toInt, s"${level}: ${msg}"))
      case _ => None
    }.toList


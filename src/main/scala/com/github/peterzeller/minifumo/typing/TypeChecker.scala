package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast

import com.github.peterzeller.minifumo.common.MinifumoError

object TypeChecker:
  final case class TypeError(message: String, source: ast.SourceRange) extends MinifumoError

  case class ExportEnv(
                      types: Map[String, String],
                      functions: Map[String, String]
                      )

  def emptyExportEnv: ExportEnv = ExportEnv(
    Map(),
    Map(),
  )

  def withStandardExports(env: ExportEnv): ExportEnv =
    ???

  def extractExports(standardProgram: ast.ProgramFile, env: ExportEnv, includeNonExported: Boolean): (ExportEnv, List[TypeError]) =
    ???

  def checkProgramWithoutStandard(program: ast.ProgramFile, importedExports: ExportEnv): (TypedAst.Program, List[TypeError]) =
    ???

  def checkProgram(program: ast.ProgramFile, importedExports: ExportEnv): (TypedAst.Program, List[TypeError]) =
    ???

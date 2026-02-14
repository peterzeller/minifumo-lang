package com.github.peterzeller.minifumo.common

import com.github.peterzeller.minifumo.ast.SourceRange

import java.nio.file.Path

case class MinifumoErrorWithPath(p: Path, err: MinifumoError)

trait MinifumoError:
  def message: String
  def source: SourceRange


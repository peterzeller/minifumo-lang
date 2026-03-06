package com.github.peterzeller.minifumo.common

import com.github.peterzeller.minifumo.ast.SourceRange

case class MinifumoErrorWithPath(p: String, err: MinifumoError)

trait MinifumoError:
  def message: String
  def source: SourceRange


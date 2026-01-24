package com.github.peterzeller.minifumo.common

import com.github.peterzeller.minifumo.ast.SourceRange

trait MinifumoError:
  def message: String
  def source: SourceRange


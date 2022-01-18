package io.github.aaronreidsmith.year2019.intcode.util

import io.github.aaronreidsmith.using
import io.github.aaronreidsmith.year2019.intcode.Instructions

trait IntCodeUtils {
  def makeInstructions(resource: String): Instructions = using(resource) { file =>
    file.mkString
      .split(',')
      .zipWithIndex
      .map { case (value, index) => index.toLong -> value.toLong }
      .toMap
  }
}

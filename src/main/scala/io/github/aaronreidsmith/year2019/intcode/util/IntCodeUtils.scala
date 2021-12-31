package io.github.aaronreidsmith.year2019.intcode.util

import io.github.aaronreidsmith.using

trait IntCodeUtils {
  def makeInstructions(resource: String): Map[Long, Long] = using(resource) { file =>
    file.mkString
      .split(',')
      .zipWithIndex
      .map { case (value, index) => index.toLong -> value.toLong }
      .toMap
  }
}

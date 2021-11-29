package io.github.aaronreidsmith.year2019.intcode.util

import scala.io.Source
import scala.util.Using

trait IntCodeUtils {
  def makeInstructions(resource: String): Map[Long, Long] = Using.resource(Source.fromResource(resource)) { file =>
    file.mkString
      .split(',')
      .zipWithIndex
      .map { case (value, index) => index.toLong -> value.toLong }
      .toMap
  }
}

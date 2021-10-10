package io.github.aaronreidsmith.year2019.intcode.util

import io.github.aaronreidsmith.util.FileUtils

import scala.io.Source

trait IntCodeUtils extends FileUtils {
  def makeInstructions(resource: String): Map[Long, Long] = using(Source.fromResource(resource)) { file =>
    file.mkString
      .split(',')
      .zipWithIndex
      .map { case (value, index) => index.toLong -> value.toLong }
      .toMap
  }
}

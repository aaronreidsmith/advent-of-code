package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.util.FileUtils

import scala.io.Source

object Day08 extends FileUtils {
  def main(args: Array[String]): Unit = {
    val width  = 25
    val height = 6
    val layers = using(Source.fromResource("2019/day08.txt")) { file =>
      file.mkString.toSeq.grouped(width).toSeq.grouped(height).toSeq
    }
    val minZeroLayer = layers.minBy(_.flatten.count(_ == '0')).flatten
    val part1        = minZeroLayer.count(_ == '1') * minZeroLayer.count(_ == '2')
    println(s"Part 1: $part1")
  }
}

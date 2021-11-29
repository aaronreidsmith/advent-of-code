package io.github.aaronreidsmith.year2019

import scala.io.Source
import scala.util.Using

object Day08 {
  def main(args: Array[String]): Unit = {
    val width           = 25
    val height          = 6
    val flattenedLayers = Using.resource(Source.fromResource("2019/day08.txt"))(_.mkString).toSeq.grouped(width * height).toList
    val minZeroLayer    = flattenedLayers.minBy(_.count(_ == '0'))
    val part1           = minZeroLayer.count(_ == '1') * minZeroLayer.count(_ == '2')
    println(s"Part 1: $part1")

    val part2 = flattenedLayers.transpose // Transpose so each sub-list is based on position
      .map { position => // Find the first non-two character in each position
        position
          .collectFirst {
            case char if char != '2' => char
          }
          .getOrElse('2')
      }
      .grouped(width) // Re-group bu width/height
      .toList
      .grouped(height)
      .toList
      .head         // After the above `collectFirst`, we will only have one outer list
      .map { row => // Collect into 1 string with only `white` showing
        row.map {
          case '1' => '#'
          case _   => ' '
        }.mkString
      }
      .mkString("\n")
    println(s"Part 2:\n$part2")
  }
}

package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day08 extends Solution(2019, 8) {
  type I  = List[String]
  type O1 = Int
  type O2 = String

  // Given
  private val width  = 25
  private val height = 6

  override protected[year2019] def parseInput(file: Source): List[String] = {
    file.mkString.trim.grouped(width * height).toList
  }

  override protected[year2019] def part1(input: List[String]): Int = {
    val minZeroLayer = input.minBy(_.count(_ == '0'))
    minZeroLayer.count(_ == '1') * minZeroLayer.count(_ == '2')
  }

  override protected[year2019] def part2(input: List[String]): String = {
    val image = input.transpose // Transpose so each sub-list is based on position
      .map { position => // Find the first non-two character in each position
        position
          .collectFirst {
            case char if char != '2' => char
          }
          .getOrElse('2')
      }
      .grouped(width) // Re-group by width/height
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

    // Need leading newline due to how `Solution` prints
    s"\n$image"
  }
}

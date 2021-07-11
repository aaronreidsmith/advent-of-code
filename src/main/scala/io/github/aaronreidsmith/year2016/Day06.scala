package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day06 {
  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("2016/day06.txt")
    val inputLines = input.getLines().map(_.toCharArray.toList).toList
    input.close()

    val part1 = inputLines.transpose.map { chars =>
      chars.groupBy(identity).toList.maxBy { case (_, occurrences) => occurrences.length }._1
    }.mkString
    println(s"Part 1: $part1")

    val part2 = inputLines.transpose.map { chars =>
      chars.groupBy(identity).toList.minBy { case (_, occurrences) => occurrences.length }._1
    }.mkString
    println(s"Part 2: $part2")
  }
}

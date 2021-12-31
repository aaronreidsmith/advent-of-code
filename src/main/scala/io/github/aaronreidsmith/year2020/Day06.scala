package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

object Day06 {
  def main(args: Array[String]): Unit = {
    val input = using("2020/day06.txt")(_.mkString.split("\n\n").toList)
    val part1 = input.foldLeft(0)(_ + _.replaceAll("\n", "").toSet.size)
    println(s"Part 1: $part1")
    val part2 = input.foldLeft(0) { (acc, entry) =>
      acc + entry.split('\n').map(_.toSet).reduceLeft(_.intersect(_)).size
    }
    println(s"Part 2: $part2")
  }
}

package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = using("2020/day01.txt")(_.getLines().map(_.toInt).toList)
    val part1 = input.combinations(2).collectFirst { case pair if pair.sum == 2020 => pair.product }.get
    println(s"Part 1: $part1")
    val part2 = input.combinations(3).collectFirst { case triplet if triplet.sum == 2020 => triplet.product }.get
    println(s"Part 2: $part2")
  }
}

package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day01.txt"))(_.getLines().map(_.toInt).toSeq)

    val part1 = input.sliding(2).count { case Seq(a, b) => b > a }
    println(s"Part 1: $part1")

    // (A + B + C) - (B + C + D) = A - D, so we only need to compare A and D
    val part2 = input.sliding(4).count { case Seq(a, _, _, d) => d > a }
    println(s"Part 2: $part2")
  }
}

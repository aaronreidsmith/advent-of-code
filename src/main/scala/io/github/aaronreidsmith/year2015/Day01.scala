package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = using("2015/day01.txt")(_.mkString)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2015] def part1(inputString: String): Int = inputString.count(_ == '(') - inputString.count(_ == ')')

  @tailrec
  private[year2015] def part2(inputString: String, level: Int = 0, position: Int = 0): Int = if (level < 0) {
    position
  } else {
    inputString.head match {
      case '(' => part2(inputString.tail, level + 1, position + 1)
      case ')' => part2(inputString.tail, level - 1, position + 1)
      case _   => throw new IllegalArgumentException
    }
  }
}

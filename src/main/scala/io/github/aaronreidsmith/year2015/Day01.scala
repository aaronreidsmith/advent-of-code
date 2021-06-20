package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val input       = Source.fromResource("2015/day01.txt")
    val inputString = input.mkString
    input.close()

    val part1 = inputString.count(_ == '(') - inputString.count(_ == ')')
    println(s"Part 1: $part1")
    println(s"Part 2: ${part2(inputString)}")
  }

  @tailrec
  private def part2(inputString: String, level: Int = 0, position: Int = 0): Int = if (level < 0) {
    position
  } else {
    inputString.head match {
      case '(' => part2(inputString.tail, level + 1, position + 1)
      case ')' => part2(inputString.tail, level - 1, position + 1)
      case _   => throw new IllegalArgumentException
    }
  }
}

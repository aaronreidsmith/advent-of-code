package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends Solution(2015, 1) {
  type I  = String
  type O1 = Int
  type O2 = Int

  override protected[year2015] def parseInput(file: Source): String = file.mkString
  override protected[year2015] def part1(input: String): Int        = input.count(_ == '(') - input.count(_ == ')')
  override protected[year2015] def part2(input: String): Int = {
    @tailrec
    def helper(current: String, level: Int = 0, position: Int = 0): Int = if (level < 0) {
      position
    } else {
      current.head match {
        case '(' => helper(current.tail, level + 1, position + 1)
        case ')' => helper(current.tail, level - 1, position + 1)
        case _   => throw new IllegalArgumentException
      }
    }

    helper(input)
  }
}

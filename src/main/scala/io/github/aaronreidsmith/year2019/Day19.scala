package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends Solution {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): IntCode = IntCode(file)

  override def part1(input: IntCode): Long = {
    for {
      x <- 0 until 50
      y <- 0 until 50
    } yield input.withInput(x, y).allOutput
  }.foldLeft(0L)(_ + _.sum)

  override def part2(input: IntCode): Long = {
    def test(x: Long, y: Long): Boolean = input.withInput(x, y).allOutput.head == 1L

    @tailrec
    def helper(x: Long, y: Long): Long = {
      val top  = test(x, y - 99)
      val left = test(x - 99, y)
      (top, left) match {
        case (true, true) => 10000 * (x - 99) + (y - 99)
        case (true, _)    => helper(x + 1, y)
        case _            => helper(x, y + 1)
      }
    }

    helper(99, 99)
  }
}

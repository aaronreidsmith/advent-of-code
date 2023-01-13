package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends Solution {
  type I  = Vector[Long]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): Vector[Long] = file.getLines().toVector.map(_.toLong)

  override def part1(input: Vector[Long]): Long = {
    @tailrec
    def helper(windowStart: Int): Long = {
      val windowEnd          = windowStart + (if (isTest) 5 else 24)
      val targetNumber       = input(windowEnd + 1)
      val preambleConditions = input.slice(windowStart, windowEnd + 1).combinations(2).map(_.sum)
      if (preambleConditions.contains(targetNumber)) {
        helper(windowStart + 1)
      } else {
        targetNumber
      }
    }

    helper(0)
  }

  override def part2(input: Vector[Long]): Long = {
    val target   = part1(input)
    val reversed = input.reverse

    @tailrec
    def helper(start: Int, end: Int): Long = {
      val range    = reversed.slice(start, end + 1)
      val rangeSum = range.sum
      if (rangeSum < target) {
        helper(start, end + 1)
      } else if (rangeSum > target) {
        helper(start + 1, start + 2)
      } else {
        range.min + range.max
      }
    }

    helper(0, 1)
  }
}

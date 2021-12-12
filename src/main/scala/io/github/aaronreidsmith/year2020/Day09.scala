package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day09 {
  def main(args: Array[String]): Unit = {
    val input = using("2020/day09.txt")(_.getLines().toVector.map(_.toLong))
    val part1 = findInvalid(input)
    println(s"Part 1: $part1")
    val part2 = findContiguousRange(input.reverse, part1)
    println(s"Part 2: $part2")
  }

  @tailrec
  private def findInvalid(input: Vector[Long], windowStart: Int = 0): Long = {
    val windowEnd          = windowStart + 24
    val targetNumber       = input(windowEnd + 1)
    val preambleConditions = input.slice(windowStart, windowEnd + 1).combinations(2).map(_.sum)
    if (preambleConditions.contains(targetNumber)) {
      findInvalid(input, windowStart + 1)
    } else {
      targetNumber
    }
  }

  @tailrec
  private def findContiguousRange(input: Vector[Long], target: Long, start: Int = 0, end: Int = 1): Long = {
    val range    = input.slice(start, end + 1)
    val rangeSum = range.sum
    if (rangeSum < target) {
      findContiguousRange(input, target, start, end + 1)
    } else if (rangeSum > target) {
      findContiguousRange(input, target, start + 1, start + 2)
    } else {
      range.min + range.max
    }
  }
}

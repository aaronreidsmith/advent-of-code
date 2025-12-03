package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day03 extends Solution {
  type I  = List[String]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[String] = {
    file.getLines().toList
  }

  override def part1(input: List[String]): Long = solution(input, 2)
  override def part2(input: List[String]): Long = solution(input, 12)

  private def solution(input: List[String], digits: Int): Long = {
    input.foldLeft(0L)((acc, bank) => acc + maxJoltage(bank, digits))
  }

  private def maxJoltage(bank: String, digits: Int): Long = {
    val cache = mutable.Map.empty[(Int, Int), Long]
    def helper(i: Int, d: Int): Long = cache.getOrElseUpdate(
      (i, d), {
        if (i >= 0 && d >= 1) {
          helper(i - 1, d).max(helper(i - 1, d - 1) * 10 + bank(i).asDigit)
        } else {
          0
        }
      }
    )

    helper(bank.length - 1, digits)
  }
}

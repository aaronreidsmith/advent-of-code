package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day15 extends Solution(2017, 15) {
  type I  = (LazyList[Long], LazyList[Long])
  type O1 = Int
  type O2 = Int

  // Hard-coded numbers are given in puzzle text
  override protected[year2017] def parseInput(file: Source): (LazyList[Long], LazyList[Long]) = {
    def generator(previous: Long, factor: Long): LazyList[Long] = {
      val next = (previous * factor) % 2147483647
      previous #:: generator(next, factor)
    }

    val aStart :: bStart :: _ = file.getLines().toList.map(_.filter(_.isDigit).toLong)
    (generator(aStart, 16807L), generator(bStart, 48271L))
  }

  override protected[year2017] def part1(input: (LazyList[Long], LazyList[Long])): Int = {
    val (a, b) = input
    a.take(40000000).lazyZip(b).count(matching)
  }

  override protected[year2017] def part2(input: (LazyList[Long], LazyList[Long])): Int = {
    val (a, b) = input
    // Our seed is divisible by 4, so we need to skip it
    // https://www.reddit.com/r/adventofcode/comments/nrfqa6/2017_day_15_part_2_scala_works_for_test_input_but/
    a.filter(_ % 4 == 0).slice(1, 5000001).lazyZip(b.filter(_ % 8 == 0)).count(matching)
  }

  private def lowest16Bits(num: Long): String = {
    val binary = num.toBinaryString
    val padded = if (binary.length < 16) s"0000000000000000$binary" else binary
    padded.takeRight(16)
  }

  private def matching(pair: (Long, Long)): Boolean = {
    val (a, b) = pair
    lowest16Bits(a) == lowest16Bits(b)
  }
}

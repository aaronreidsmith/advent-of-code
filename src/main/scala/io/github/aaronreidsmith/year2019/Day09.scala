package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day09 extends Solution(2019, 9) {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override protected[year2019] def parseInput(file: Source): IntCode = IntCode(file)
  override protected[year2019] def part1(input: IntCode): Long       = input.withInput(1L).allOutput.last
  override protected[year2019] def part2(input: IntCode): Long       = input.withInput(2L).allOutput.last
}

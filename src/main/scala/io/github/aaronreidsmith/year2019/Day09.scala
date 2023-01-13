package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day09 extends Solution {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): IntCode = IntCode(file)
  override def part1(input: IntCode): Long       = input.withInput(1L).allOutput.last
  override def part2(input: IntCode): Long       = input.withInput(2L).allOutput.last
}

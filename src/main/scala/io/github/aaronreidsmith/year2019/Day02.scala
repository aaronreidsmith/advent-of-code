package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day02 extends Solution {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): IntCode = IntCode(file)

  override def part1(intcode: IntCode): Long = {
    val updated = intcode.copy(memory = intcode.memory ++ Map(1L -> 12L, 2L -> 2L)) // From problem
    updated.nextOutput.memory(0L)
  }

  override def part2(input: IntCode): Long = {
    for {
      noun <- 0L until 100L
      verb <- 0L until 100L
      updated = input.copy(memory = input.memory ++ Map(1L -> noun, 2L -> verb))
      if updated.nextOutput.memory(0L) == 19690720L
    } yield (100 * noun) + verb
  }.head
}

package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils
import io.github.aaronreidsmith.year2019.intcode.{Instructions, IntCode}

import scala.io.Source

object Day02 extends Solution(2019, 2) with IntCodeUtils {
  type I  = Instructions
  type O1 = Long
  type O2 = Long

  override protected[year2019] def parseInput(file: Source): Instructions = file.toInstructions

  override protected[year2019] def part1(instructions: Instructions): Long = {
    val updatedInstructions = instructions ++ Map(1L -> 12L, 2L -> 2L) // From problem
    val intCode             = new IntCode(updatedInstructions)
    intCode.run().getRegisterValue(0)
  }

  override protected[year2019] def part2(instructions: Instructions): Long = {
    for {
      noun <- 0L until 100L
      verb <- 0L until 100L
    } yield (noun, verb, instructions ++ Map(1L -> noun, 2L -> verb))
  }.collectFirst {
    case (noun, verb, updatedInstructions) if new IntCode(updatedInstructions).run().getRegisterValue(0) == 19690720 =>
      (100 * noun) + verb
  }.getOrElse(-1)
}

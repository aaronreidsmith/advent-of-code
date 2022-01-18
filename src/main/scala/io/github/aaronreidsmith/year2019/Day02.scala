package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils
import io.github.aaronreidsmith.year2019.intcode.{Instructions, IntCode}

object Day02 extends IntCodeUtils {
  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day02.txt")
    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: ${part2(instructions)}")
  }

  private[year2019] def part1(instructions: Instructions): Long = {
    val updatedInstructions = instructions ++ Map(1L -> 12L, 2L -> 2L) // From problem
    val intCode             = new IntCode(updatedInstructions)
    intCode.run().getRegisterValue(0)
  }

  private[year2019] def part2(instructions: Instructions): Long = {
    for {
      noun <- 0L until 100L
      verb <- 0L until 100L
    } yield (noun, verb, instructions ++ Map(1L -> noun, 2L -> verb))
  }.collectFirst {
    case (noun, verb, updatedInstructions) if new IntCode(updatedInstructions).run().getRegisterValue(0) == 19690720 =>
      (100 * noun) + verb
  }.getOrElse(-1)
}

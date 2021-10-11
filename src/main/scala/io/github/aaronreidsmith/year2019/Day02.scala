package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

object Day02 extends IntCodeUtils {
  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day02.txt")

    val part1Instructions = instructions ++ Map(1L -> 12L, 2L -> 2L) // From problem
    val part1IntCode      = new IntCode(part1Instructions)
    println(s"Part 1: ${part1IntCode.run().getRegisterValue(0)}")

    val part2 = {
      for {
        noun <- 0L until 100L
        verb <- 0L until 100L
      } yield (noun, verb, instructions ++ Map(1L -> noun, 2L -> verb))
    }.collectFirst {
      case (noun, verb, updatedInstructions)
          if new IntCode(updatedInstructions).run().getRegisterValue(0) == 19690720 =>
        (100 * noun) + verb
    }.get
    println(s"Part 2: $part2")
  }
}

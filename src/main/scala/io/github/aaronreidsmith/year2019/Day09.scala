package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

object Day09 extends IntCodeUtils {
  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day09.txt")

    val part1 = new IntCode(instructions, Seq(1L))
    println(s"Part 1: ${part1.run().getOutputAsString}")

    val part2 = new IntCode(instructions, Seq(2L))
    println(s"Part 2: ${part2.run().getOutputAsString}")
  }
}

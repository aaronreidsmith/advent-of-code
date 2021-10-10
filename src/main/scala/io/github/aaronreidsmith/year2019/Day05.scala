package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

object Day05 extends IntCodeUtils {
  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day05.txt")

    println("=== Part 1 ===")
    val part1 = new IntCode(instructions, Some(1L))
    part1.run()

    println("=== Part 2 ===")
    val part2 = new IntCode(instructions, Some(5L))
    part2.run()
  }
}

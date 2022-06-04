package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using
import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils
import net.fornwall.aoc.Solver

object Day19 extends IntCodeUtils {
  def main(args: Array[String]): Unit = {
    val rawInstructions = using("2019/day19.txt")(_.mkString)
    val instructions    = makeInstructions("2019/day19.txt")

    val part1 = {
      for {
        x <- 0 until 50
        y <- 0 until 50
        if new IntCode(instructions, Seq(x, y)).run().getOutput.last == 1
      } yield 1
    }.sum
    println(s"Part 1: $part1")

    // TODO: Actually solve this
    println(s"Part 2: ${Solver.solve(2019, 19, 2, rawInstructions)}")
  }
}

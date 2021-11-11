package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

object Day19 extends IntCodeUtils {
  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day19.txt")

    val part1 = {
      for {
        x <- 0 until 50
        y <- 0 until 50
        if new IntCode(instructions, Seq(x, y)).run().getOutput.last == 1
      } yield 1
    }.sum
    println(part1)

    val tractorBeam = {
      for {
        x <- 0 until 10000
        y <- 0 until 10000
        if new IntCode(instructions, Seq(x, y)).run().getOutput.last == 1
      } yield (x, y)
    }.toSet
    val (part2X, part2Y) = tractorBeam.filter {
      case (x, y) =>
        tractorBeam.contains((x + 100, y)) &&
          tractorBeam.contains((x, y + 100)) &&
          tractorBeam.contains((x + 100, y + 100))
    }.min
    println(s"Part 2: ${part2X * 10000 + part2Y}")
  }
}

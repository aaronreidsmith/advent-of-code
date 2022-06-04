package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using
import net.fornwall.aoc.Solver

// TODO: Actually solve this
object Day25 {
  def main(args: Array[String]): Unit = {
    val input = using("2019/day25.txt")(_.mkString)
    println(s"Part 1: ${Solver.solve(2019, 25, 1, input)}")
  }
}

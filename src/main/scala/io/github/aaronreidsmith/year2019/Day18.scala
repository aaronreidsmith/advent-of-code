package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using
import net.fornwall.aoc.Solver

// TODO: Actually solve this
object Day18 {
  def main(args: Array[String]): Unit = {
    val input = using("2019/day18.txt")(_.mkString)
    println(s"Part 1: ${Solver.solve(2019, 18, 1, input)}")
    println(s"Part 2: ${Solver.solve(2019, 18, 2, input)}")
  }
}

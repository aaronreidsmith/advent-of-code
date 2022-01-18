package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using

import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = using("2019/day01.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2019] def parseInput(file: Source): List[Int] = file.getLines().toList.map(_.toInt)

  private[year2019] def part1(modules: List[Int]): Int = modules.foldLeft(0) { (acc, module) =>
    val requiredFuel = (module / 3) - 2
    acc + requiredFuel
  }

  private[year2019] def part2(modules: List[Int]): Int = {
    def helper(module: Int): Int = {
      val requiredFuel = (module / 3) - 2
      if (requiredFuel > 8) {
        requiredFuel + helper(requiredFuel)
      } else {
        requiredFuel
      }
    }

    modules.foldLeft(0)((acc, module) => acc + helper(module))
  }
}

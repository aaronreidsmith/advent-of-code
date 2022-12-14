package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day01 extends Solution(2019, 1) {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override protected[year2019] def parseInput(file: Source): List[Int] = file.getLines().toList.map(_.toInt)

  override protected[year2019] def part1(modules: List[Int]): Int = modules.foldLeft(0) { (acc, module) =>
    val requiredFuel = (module / 3) - 2
    acc + requiredFuel
  }

  override protected[year2019] def part2(modules: List[Int]): Int = {
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

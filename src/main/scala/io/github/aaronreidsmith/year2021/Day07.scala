package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.util.Using

object Day07 extends Solution {
  type I  = Boolean => Int
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Boolean => Int = {
    val crabs = file.mkString.trim.split(',').map(_.toInt).toSeq
    (part2: Boolean) =>
      (0 to crabs.max).foldLeft(Int.MaxValue) { (acc, n) =>
        val fuelUsed = crabs.foldLeft(0) { (fuelAcc, crab) =>
          val moves = (crab - n).abs
          if (part2) {
            val fuelUsed = (math.pow(moves, 2).toInt + moves) / 2 // Gauss formula: https://nrich.maths.org/2478
            fuelAcc + fuelUsed
          } else {
            fuelAcc + moves
          }
        }
        if (fuelUsed < acc) fuelUsed else acc
      }
  }

  override def part1(input: Boolean => Int): Int = input(false)
  override def part2(input: Boolean => Int): Int = input(true)
}

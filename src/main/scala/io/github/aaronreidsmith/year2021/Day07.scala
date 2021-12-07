package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day07 {
  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day07.txt"))(_.mkString.split(',').map(_.toInt).toSeq)
    println(s"Part 1: ${solution(input, part2 = false)}")
    println(s"Part 1: ${solution(input, part2 = true)}")
  }

  private def solution(crabs: Seq[Int], part2: Boolean): Int = (0 to crabs.max).foldLeft(Int.MaxValue) { (acc, n) =>
    val fuelUsed = crabs.foldLeft(0) {
      case (fuelAcc, crab) if part2 =>
        val moves    = (crab - n).abs
        val fuelUsed = (math.pow(moves, 2).toInt + moves) / 2 // Gauss formula: https://nrich.maths.org/2478
        fuelAcc + fuelUsed
      case (fuelAcc, crab) => fuelAcc + (crab - n).abs
    }
    if (fuelUsed < acc) fuelUsed else acc
  }
}

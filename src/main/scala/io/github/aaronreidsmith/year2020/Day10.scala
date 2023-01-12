package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day10 extends Solution(2020, 10) {
  type I  = Vector[Int]
  type O1 = Int
  type O2 = Long

  override protected[year2020] def parseInput(file: Source): Vector[Int] = {
    val adaptors      = file.getLines().toVector.map(_.toInt).sorted
    val maxJoltage    = adaptors.last
    val deviceJoltage = maxJoltage + 3
    0 +: adaptors :+ deviceJoltage
  }

  override protected[year2020] def part1(input: Vector[Int]): Int = {
    @tailrec
    def helper(remaining: Vector[Int], differences: List[Int] = Nil): Int = remaining match {
      case Vector(_)        => differences.count(_ == 1) * differences.count(_ == 3)
      case Vector(a, b, _*) => helper(remaining.tail, (b - a) :: differences)
      case _                => throw new IllegalArgumentException
    }

    helper(input)
  }

  override protected[year2020] def part2(input: Vector[Int]): Long = {
    val maxJoltage = input.last - 3 // Device joltage minus 3
    val cache      = mutable.Map.empty[Int, Long]
    def helper(currentJoltage: Int): Long = cache.getOrElseUpdate(
      currentJoltage,
      if (currentJoltage == maxJoltage) {
        1
      } else if (!input.contains(currentJoltage)) {
        0
      } else {
        helper(currentJoltage + 1) + helper(currentJoltage + 2) + helper(currentJoltage + 3)
      }
    )

    helper(0)
  }
}

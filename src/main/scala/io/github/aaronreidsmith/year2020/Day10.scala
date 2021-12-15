package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.collection.mutable

object Day10 {
  def main(args: Array[String]): Unit = {
    val adaptors      = using("2020/day10.txt")(_.getLines().toVector.map(_.toInt).sorted)
    val maxJoltage    = adaptors.last
    val deviceJoltage = maxJoltage + 3
    val joltages      = 0 +: adaptors :+ deviceJoltage

    @tailrec
    def part1(remaining: Vector[Int], differences: Vector[Int] = Vector()): Int = remaining match {
      case Vector(_)        => differences.count(_ == 1) * differences.count(_ == 3)
      case Vector(a, b, _*) => part1(remaining.tail, differences :+ (b - a))
    }
    println(s"Part 1: ${part1(joltages)}")

    val cache = mutable.Map.empty[Int, Long]
    def part2(currentJoltage: Int): Long = cache.getOrElseUpdate(
      currentJoltage,
      if (currentJoltage == maxJoltage) 1
      else if (!joltages.contains(currentJoltage)) 0
      else part2(currentJoltage + 1) + part2(currentJoltage + 2) + part2(currentJoltage + 3)
    )
    println(s"Part 2: ${part2(0)}")
  }
}

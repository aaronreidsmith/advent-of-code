package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using

object Day05 {
  def main(args: Array[String]): Unit = {
    val polymer = using("2018/day05.txt")(_.mkString)
    println(s"Part 1: ${part1(polymer)}")
    println(s"Part 2: ${part2(polymer)}")
  }

  private[year2018] def part1(polymer: String): Int = react(polymer).length

  private[year2018] def part2(polymer: String): Int = {
    val preReacted = react(polymer)
    val units      = preReacted.toLowerCase.toSet
    units.foldLeft(Int.MaxValue) { (currentBest, unit) =>
      val testPolymer = preReacted.filterNot(_.toLower == unit)
      currentBest.min(part1(testPolymer))
    }
  }

  private def react(polymer: String): String = polymer
    .foldLeft(List.empty[Char]) {
      case (a :: tail, b) if opposites(a, b) => tail
      case (acc, b)                          => b :: acc
    }
    .reverse
    .mkString

  private def opposites(a: Char, b: Char): Boolean = a != b && a.toLower == b.toLower
}

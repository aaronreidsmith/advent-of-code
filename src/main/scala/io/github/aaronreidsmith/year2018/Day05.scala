package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day05 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(polymer: String): Int      = react(polymer).length
  override def part2(polymer: String): Int = {
    val preReacted = react(polymer)
    val units      = preReacted.toLowerCase.toSet
    units.foldLeft(Int.MaxValue) { (currentBest, unit) =>
      val testPolymer = preReacted.filterNot(_.toLower == unit)
      currentBest.min(part1(testPolymer))
    }
  }

  private def react(polymer: String): String = {
    polymer
      .foldLeft(List.empty[Char]) {
        case (a :: tail, b) if a != b && a.toLower == b.toLower => tail
        case (acc, b)                                           => b :: acc
      }
      .reverse
      .mkString
  }
}

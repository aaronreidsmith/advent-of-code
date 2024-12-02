package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day02 extends Solution {
  type I  = List[List[Int]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[List[Int]] = {
    file.getLines().toList.map(line => line.split(' ').toList.map(_.toInt))
  }

  override def part1(input: List[List[Int]]): Int = {
    input.count(isSafe)
  }

  override def part2(input: List[List[Int]]): Int = {
    // Scala .combinations() doesn't handle duplicates 🥲
    def combinations(report: List[Int]): Seq[List[Int]] = {
      report.indices.map(i => report.take(i) ++ report.drop(i + 1))
    }

    input.count(report => isSafe(report) || combinations(report).exists(isSafe))
  }

  private def isSafe(report: List[Int]): Boolean = {
    val zipped = report.init.zip(report.tail)
    (zipped.forall(_ < _) || zipped.forall(_ > _)) && zipped.forall((a, b) => 1 <= (a - b).abs && (a - b).abs <= 3)
  }
}

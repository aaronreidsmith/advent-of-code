package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.extensions.*

import scala.io.Source

object Day01 extends Solution {
  type I  = (List[Int], List[Int])
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): (List[Int], List[Int]) = {
    file
      .getLines()
      .map { line =>
        val Array(a, b, _*) = line.split("""\s+"""): @unchecked
        (a.toInt, b.toInt)
      }
      .toList
      .unzip
  }

  override def part1(input: (List[Int], List[Int])): Int = {
    val (left, right) = input
    left.sorted.zip(right.sorted).foldLeft(0) { case (acc, (a, b)) => acc + (a - b).abs }
  }

  override def part2(input: (List[Int], List[Int])): Int = {
    val (left, right)    = input
    val rightOccurrences = right.occurrences
    left.foldLeft(0)((acc, value) => acc + (value * rightOccurrences.getOrElse(value, 0)))
  }
}

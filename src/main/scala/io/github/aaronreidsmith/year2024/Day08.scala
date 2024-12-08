package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.io.Source

object Day08 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Day08.I = file.toGrid

  override def part1(input: Grid[Char]): Int = solution(input, Range(1, 2))
  override def part2(input: Grid[Char]): Int = solution(input, Range(0, 50))

  private def solution(input: Grid[Char], distances: Range): Int = {
    val antiNodes = input.values.toSet.removedAll(Set('.')).foldLeft(Set.empty[Point]) { (acc, frequency) =>
      val antennaPairs = input
        .collect { case (pos, char) if char == frequency => pos }
        .toList
        .combinations(2)
        .flatMap(_.permutations)
        .toList
      acc ++ distances.flatMap(n => antennaPairs.collect { case List(a, b) => a + (n * (a - b)) })
    }
    antiNodes.intersect(input.keySet).size
  }
}

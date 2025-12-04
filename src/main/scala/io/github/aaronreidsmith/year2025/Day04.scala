package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day04 extends Solution {
  type I  = Set[Point]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Set[Point] = {
    file.toGrid.filterNot((_, char) => char == '.').keySet
  }

  override def part1(input: Set[Point]): Int = {
    input.count(isAccessible(input, _))
  }

  override def part2(input: Set[Point]): Int = {
    @tailrec
    def helper(grid: Set[Point], removed: Int = 0): Int = {
      val canBeRemoved = grid.filter(isAccessible(grid, _))
      if (canBeRemoved.isEmpty) {
        removed
      } else {
        helper(grid -- canBeRemoved, removed + canBeRemoved.size)
      }
    }

    helper(input)
  }

  private def isAccessible(grid: Set[Point], point: Point): Boolean = {
    point.neighbors.count(grid.contains) < 4
  }
}

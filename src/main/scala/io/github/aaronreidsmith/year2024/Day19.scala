package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day19 extends Solution {
  type I  = (List[String], List[String])
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): (List[String], List[String]) = {
    val patterns :: _ :: designs = file.getLines().toList: @unchecked
    (patterns.split(", ").toList, designs)
  }

  override def part1(input: (List[String], List[String])): Int = {
    solution.tupled(input).count(_ != 0)
  }

  override def part2(input: (List[String], List[String])): Long = {
    solution.tupled(input).sum
  }

  private def solution(patterns: List[String], designs: List[String]): List[Long] = {
    val cache = mutable.Map.empty[String, Long]
    def helper(design: String): Long = cache.getOrElseUpdate(
      design, {
        if (design.isEmpty) {
          1L
        } else {
          patterns.foldLeft(0L) {
            case (acc, pattern) if design.startsWith(pattern) => acc + helper(design.stripPrefix(pattern))
            case (acc, _)                                     => acc
          }
        }
      }
    )

    designs.map(helper)
  }
}

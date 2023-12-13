package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

// Adapted from https://github.com/NickyMeuleman/scrapyard/blob/77b1ab12cf004d2013dcf334ac71544fc56f3dae/advent_of_code/2023/solutions/src/day_12.rs
object Day12 extends Solution {
  type I  = List[(String, List[Int])]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[(String, List[Int])] = file.getLines().toList.map { line =>
    val Array(springs, countsRaw, _*) = line.split(' '): @unchecked
    (springs, countsRaw.split(',').toList.map(_.toInt))
  }

  override def part1(input: List[(String, List[Int])]): Long = solution(input)

  override def part2(input: List[(String, List[Int])]): Long = {
    val updated = input.map { (springs, counts) =>
      (List.fill(5)(springs).mkString("?"), List.fill(5)(counts).flatten)
    }
    solution(updated)
  }

  private def solution(input: List[(String, List[Int])]): Long = {
    val cache = mutable.Map.empty[(String, List[Int]), Long]
    def helper(springs: String, counts: List[Int]): Long = cache.getOrElseUpdate(
      (springs, counts), {
        if (counts.isEmpty) {
          if (springs.contains('#')) 0 else 1
        } else if (springs.length < counts.sum + counts.length) {
          0
        } else {
          var arrangements = 0L
          if (springs.head != '#') {
            arrangements += helper(springs.tail, counts)
          }
          val nextGroupSize = counts.head
          if (!springs.take(nextGroupSize).contains('.') && springs(nextGroupSize) != '#') {
            arrangements += helper(springs.drop(nextGroupSize + 1), counts.tail)
          }
          arrangements
        }
      }
    )

    input.foldLeft(0L) {
      case (acc, (springs, counts)) => acc + helper(springs + ".", counts)
    }
  }
}

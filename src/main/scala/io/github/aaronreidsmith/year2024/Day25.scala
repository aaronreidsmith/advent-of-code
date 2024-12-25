package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day25 extends Solution {
  type I  = (List[Set[Point]], List[Set[Point]])
  type O1 = Int
  type O2 = Nothing

  override def parseInput(file: Source): (List[Set[Point]], List[Set[Point]]) = {
    file.mkString.split("\n\n").foldLeft((List.empty[Set[Point]], List.empty[Set[Point]])) {
      case ((locks, keys), block) =>
        val lockOrKey = block.toGrid.filterNot(_._2 == '.').keySet
        if (block.head == '#') {
          (lockOrKey :: locks, keys)
        } else {
          (locks, lockOrKey :: keys)
        }
    }
  }

  override def part1(input: (List[Set[Point]], List[Set[Point]])): Int = {
    val (locks, keys) = input
    locks.foldLeft(0)((acc, lock) => acc + keys.count(key => lock.intersect(key).isEmpty))
  }
}

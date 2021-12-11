package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith._

import scala.annotation.tailrec

object Day03 {
  def main(args: Array[String]): Unit = {
    val input = using("2020/day03.txt") { file =>
      file.getLines().toList.map { line =>
        LazyList.continually(line.toList).flatten
      }
    }
    val part1 = Seq((1, 3)).map(traverse(input, _)).product
    println(s"Part 2: $part1")
    val part2 = Seq((1, 1), (1, 3), (1, 5), (1, 7), (2, 1)).map(traverse(input, _)).product
    println(s"Part 2: $part2")
  }

  @tailrec
  private def traverse(
      mountain: List[LazyList[Char]],
      increments: (Int, Int),
      treesEncountered: Int = 0,
      position: (Int, Int) = (0, 0)
  ): Int = {
    val (row, col) = position
    if (row >= mountain.size) {
      treesEncountered
    } else {
      val (dx, dy) = increments
      val treesHit = if (mountain(row)(col) == '#') 1 else 0
      traverse(mountain, increments, treesEncountered + treesHit, (row + dx, col + dy))
    }
  }
}

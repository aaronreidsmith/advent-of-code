package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends Solution {
  type I  = List[LazyList[Char]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[LazyList[Char]] = file.getLines().toList.map { line =>
    LazyList.continually(line).flatten
  }

  override def part1(input: List[LazyList[Char]]): Int = traverse(input, (1, 3))
  override def part2(input: List[LazyList[Char]]): Int = {
    Seq((1, 1), (1, 3), (1, 5), (1, 7), (2, 1)).foldLeft(1)(_ * traverse(input, _))
  }

  @tailrec
  private def traverse(
      mountain: List[LazyList[Char]],
      increments: (Int, Int),
      treesEncountered: Int = 0,
      position: Point = Point.zero
  ): Int = if (position.x >= mountain.size) {
    treesEncountered
  } else {
    val treesHit = if (mountain(position.x)(position.y) == '#') 1 else 0
    traverse(mountain, increments, treesEncountered + treesHit, position + increments)
  }
}

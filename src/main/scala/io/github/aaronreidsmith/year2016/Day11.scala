package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  // Just returns number of items per floor; actual items don't matter
  override def parseInput(file: Source): List[Int] = {
    val itemsOfInterest = "generator|microchip".r
    file.getLines().toList.map(line => itemsOfInterest.findAllIn(line).size)
  }

  override def part1(input: List[Int]): Int = solution(input)

  // Part 2 is same as part 2 with different input. Updated to add 4 new items to first floor
  override def part2(input: List[Int]): Int = part1(12 :: input.tail)

  // Adapted from https://www.reddit.com/r/adventofcode/comments/5hoia9/comment/db1v0hi/ with some bug fixes
  @tailrec
  private def solution(items: Seq[Int], moves: Int = 0): Int = if (items.last == items.sum) {
    moves - 1
  } else {
    val lowestFloor  = items.zipWithIndex.collectFirst { case (itemCount, index) if itemCount != 0 => index }.get
    val updatedMoves = moves + (2 * items(lowestFloor))
    val updatedItems = items.updated(lowestFloor + 1, items(lowestFloor)).updated(lowestFloor, 0)
    solution(updatedItems, updatedMoves)
  }
}

package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec

object Day11 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2016, Day 11")
    val input = List(8, 2, 0, 0) // 8 items on first floor and 2 items on second floor. Actual items don't matter
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(12 :: input.tail)}") // Updated to add the 4 new items on the first floor
    println()
  }

  override protected[year2016] def part1(input: List[Int]): Int = solution(input)

  // Part 2 is same as part 2 with different input
  override protected[year2016] def part2(input: List[Int]): Int = part1(input)

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

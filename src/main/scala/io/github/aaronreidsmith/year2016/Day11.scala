package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    val part1State = Seq(8, 2, 0, 0) // 8 items on first floor and 2 items on second floor. Actual items don't matter
    println(s"Part 1: ${solution(part1State)}")

    val part2State = Seq(12, 2, 0, 0) // Updated to add the 4 new items on the first floor
    println(s"Part 2: ${solution(part2State)}")
  }

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

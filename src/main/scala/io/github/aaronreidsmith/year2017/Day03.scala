package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Direction, East, North, Point, Solution, South, West}

import scala.annotation.tailrec

object Day03 extends Solution {
  type I  = Int
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2017, Day 3")
    val input = 347991
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2017] def part1(input: Int): Int = {
    solution(input, Map(Point(0, 0) -> 1, Point(1, 0) -> 2), part1 = true)
  }

  override protected[year2017] def part2(input: Int): Int = {
    solution(input, Map(Point(0, 0) -> 1, Point(1, 0) -> 1), part1 = false)
  }

  // Start in second square for ease of algorithm
  @tailrec
  private def solution(
      target: Int,
      grid: Map[Point, Int],
      part1: Boolean,
      pos: Point = Point(1, 0),
      acc: Int = 2,
      direction: Direction = North
  ): Int = {
    if (acc == target && part1) {
      pos.manhattanDistance(Point.zero)
    } else if (acc > target && !part1) {
      acc
    } else {
      val squareToLeft = direction match {
        case North => pos.left
        case East  => pos.up
        case South => pos.right
        case West  => pos.down
      }
      val newDirection = grid.get(squareToLeft) match {
        case Some(_) => direction      // If there is something to our left, keep going in same direction
        case None    => direction.left // Otherwise rotate counter clockwise
      }
      val newPos = newDirection match {
        case North => pos.up
        case East  => pos.right
        case South => pos.down
        case West  => pos.left
      }
      val newNum  = if (part1) acc + 1 else pos.neighbors.flatMap(grid.get).sum
      val newGrid = grid + (pos -> newNum)
      solution(target, newGrid, part1, newPos, newNum, newDirection)
    }
  }
}

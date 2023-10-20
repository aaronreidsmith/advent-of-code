package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.*

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends Solution {
  type I  = Int
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Int = file.mkString.trim.toInt

  override def part1(input: Int): Int = {
    solution(input, Map(Point(0, 0) -> 1, Point(0, 1) -> 2), part1 = true)
  }

  override def part2(input: Int): Int = {
    solution(input, Map(Point(0, 0) -> 1, Point(0, 1) -> 1), part1 = false)
  }

  // Start in second square for ease of algorithm
  @tailrec
  private def solution(
      target: Int,
      grid: Map[Point, Int],
      part1: Boolean,
      pos: Point = Point(0, 1),
      acc: Int = 2,
      direction: Direction = Direction.North
  ): Int = {
    if (acc == target && part1) {
      pos.manhattanDistance(Point.zero)
    } else if (acc > target && !part1) {
      acc
    } else {
      val squareToLeft = direction match {
        case Direction.North => pos.left
        case Direction.East  => pos.up
        case Direction.South => pos.right
        case Direction.West  => pos.down
      }
      val newDirection = grid.get(squareToLeft) match {
        case Some(_) => direction      // If there is something to our left, keep going in same direction
        case None    => direction.left // Otherwise rotate counter clockwise
      }
      val newPos = newDirection match {
        case Direction.North => pos.up
        case Direction.East  => pos.right
        case Direction.South => pos.down
        case Direction.West  => pos.left
      }
      val newNum  = if (part1) acc + 1 else pos.neighbors.flatMap(grid.get).sum
      val newGrid = grid + (pos -> newNum)
      solution(target, newGrid, part1, newPos, newNum, newDirection)
    }
  }
}

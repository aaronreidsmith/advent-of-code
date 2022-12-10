package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.implicits.SourceOps
import io.github.aaronreidsmith.{Direction, East, Grid, North, Point, Solution, South, West}

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends Solution(2017, 19) {
  type I  = Grid[Char]
  type O1 = String
  type O2 = Int

  override protected[year2017] def parseInput(file: Source): Grid[Char] = file.toGrid
  override protected[year2017] def part1(input: Grid[Char]): String     = solution(input)._1
  override protected[year2017] def part2(input: Grid[Char]): Int        = solution(input)._2

  private var answer = ("", 0)
  private var solved = false
  private def solution(grid: Grid[Char]): (String, Int) = {
    def turn(currentDirection: Direction, pos: Point): (Direction, Point) = currentDirection match {
      case North | South =>
        grid.get(pos.left) match {
          case Some(char) if char != ' ' => (West, pos.left)
          case _ =>
            grid.get(pos.right) match {
              case Some(char) if char != ' ' => (East, pos.right)
              case _                         => throw new IllegalArgumentException
            }
        }
      case East | West =>
        grid.get(pos.up) match {
          case Some(char) if char != ' ' => (North, pos.up)
          case _ =>
            grid.get(pos.down) match {
              case Some(char) if char != ' ' => (South, pos.down)
              case _                         => throw new IllegalArgumentException
            }
        }
    }

    @tailrec
    def helper(
        position: Point,
        direction: Direction = South,
        lettersSeen: StringBuilder = new StringBuilder,
        stepCount: Int = 0
    ): (String, Int) = grid.get(position) match {
      case Some(char) if char != ' ' =>
        char match {
          // Turn the only available direction
          case '+' =>
            val (newDirection, newPosition) = turn(direction, position)
            helper(newPosition, newDirection, lettersSeen, stepCount + 1)
          case other =>
            val newPosition = direction match {
              case North => position.up
              case East  => position.right
              case South => position.down
              case West  => position.left
            }
            val addedLetter = if (other == '|' | other == '-') "" else other.toString
            helper(newPosition, direction, lettersSeen.addAll(addedLetter), stepCount + 1)
        }
      case _ => (lettersSeen.result(), stepCount)
    }

    if (!solved) {
      val start = grid.collectFirst { case (pos, char) if pos.x == 0 && char == '|' => pos }.get
      answer = helper(start)
      solved = true
    }

    answer
  }
}

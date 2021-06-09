package io.github.aaronreidsmith.year2017

import scala.annotation.tailrec
import scala.io.Source

object Day19 {
  object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
  }
  import Direction._

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2017/day19.txt")
    val grid = input.getLines().zipWithIndex.foldLeft(Map.empty[(Int, Int), Char]) {
      case (acc, (line, i)) =>
        val row = line.zipWithIndex.foldLeft(Map.empty[(Int, Int), Char]) {
          case (rowAcc, (char, j)) => rowAcc + ((i, j) -> char)
        }
        acc ++ row
    }
    input.close()
    val (startX, startY) = grid.collectFirst {
      case ((row, column), char) if row == 0 && char == '|' => (row, column)
    }.get

    val (lettersSeen, stepCount) = solution(startX, startY, grid)
    println(s"Part 1: $lettersSeen")
    println(s"Part 2: $stepCount")
  }

  @tailrec
  def solution(
      row: Int,
      col: Int,
      grid: Map[(Int, Int), Char],
      direction: Direction = Down,
      lettersSeen: String = "",
      stepCount: Int = 0
  ): (String, Int) = grid.get((row, col)) match {
    case Some(char) if char != ' ' =>
      char match {
        // Turn the only available direction
        case '+' =>
          val (newDirection, newRow, newCol) = turn(direction, grid, row, col)
          solution(newRow, newCol, grid, newDirection, lettersSeen, stepCount + 1)
        // Keep going in same direction and collect letter
        case other =>
          val (newRow, newY) = direction match {
            case Down  => (row + 1, col)
            case Up    => (row - 1, col)
            case Left  => (row, col - 1)
            case Right => (row, col + 1)
          }
          val addedLetter = if (other == '|' | other == '-') "" else other.toString
          solution(newRow, newY, grid, direction, lettersSeen + addedLetter, stepCount + 1)
      }
    case _ => (lettersSeen, stepCount)
  }

  private def turn(
      currentDirection: Direction,
      grid: Map[(Int, Int), Char],
      row: Int,
      col: Int
  ): (Direction, Int, Int) =
    currentDirection match {
      case Down | Up =>
        grid.get((row, col - 1)) match {
          case Some(char) if char != ' ' => (Left, row, col - 1)
          case _ =>
            grid.get((row, col + 1)) match {
              case Some(char) if char != ' ' => (Right, row, col + 1)
              case _                         => (currentDirection, -1, -1)
            }
        }
      case Left | Right =>
        grid.get((row + 1, col)) match {
          case Some(char) if char != ' ' => (Down, row + 1, col)
          case _ =>
            grid.get((row - 1, col)) match {
              case Some(char) if char != ' ' => (Up, row - 1, col)
              case _                         => (currentDirection, -1, -1)
            }
        }
    }
}

package io.github.aaronreidsmith.year2017

import scala.annotation.tailrec
import scala.io.Source

object Day22 {
  private object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
  }
  import Direction._

  private object State extends Enumeration {
    type State = Value
    val Clean, Weakened, Flagged, Infected = Value
  }
  import State._

  private def toState(char: Char): State = char match {
    case '.'   => Clean
    case 'W'   => Weakened
    case 'F'   => Flagged
    case '#'   => Infected
    case other => throw new IllegalArgumentException(other.toString)
  }

  private case class Node(direction: Direction, row: Int, col: Int)

  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("2017/day22.txt")
    val inputLines = input.getLines().toList
    input.close()

    val grid = inputLines.zipWithIndex.foldLeft(Map.empty[(Int, Int), State]) {
      case (acc, (line, row)) =>
        acc ++ line.zipWithIndex.foldLeft(Map.empty[(Int, Int), State]) {
          case (rowAcc, (char, col)) => rowAcc + ((row, col) -> toState(char))
        }
    }

    val startRow    = inputLines.size / 2
    val startCol    = startRow
    val initialNode = Node(Up, startRow, startCol)

    println(s"Part 1: ${part1(initialNode, grid)}")
    println(s"Part 2: ${part2(initialNode, grid)}")
  }

  @tailrec
  def part1(node: Node, grid: Map[(Int, Int), State], iteration: Int = 0, nodesInfected: Int = 0): Int =
    if (iteration >= 10000) {
      nodesInfected
    } else {
      import node._
      grid.get((row, col)) match {
        case Some(state) =>
          val isInfected        = state == Infected
          val nextDirection     = if (isInfected) turnRight(direction) else turnLeft(direction)
          val (newRow, newCol)  = stepForward(nextDirection, row, col)
          val newState          = if (isInfected) Clean else Infected
          val newInfectionCount = if (newState == Infected) 1 else 0
          part1(
            Node(nextDirection, newRow, newCol),
            grid.updated((row, col), newState),
            iteration + 1,
            nodesInfected + newInfectionCount
          )
        // An unknown node, but we assume it is uninfected
        case None =>
          val nextDirection    = turnLeft(direction)
          val (newRow, newCol) = stepForward(nextDirection, row, col)
          val newState         = Infected
          part1(
            Node(nextDirection, newRow, newCol),
            grid.updated((row, col), newState),
            iteration + 1,
            nodesInfected + 1
          )
      }
    }

  @tailrec
  private def part2(node: Node, grid: Map[(Int, Int), State], iteration: Int = 0, nodesInfected: Int = 0): Int =
    if (iteration >= 10000000) {
      nodesInfected
    } else {
      import node._
      grid.get((row, col)) match {
        case Some(state) =>
          val nextDirection = state match {
            case Clean    => turnLeft(direction)
            case Weakened => direction
            case Infected => turnRight(direction)
            case Flagged  => reverse(direction)
          }
          val newState = state match {
            case Clean    => Weakened
            case Weakened => Infected
            case Infected => Flagged
            case Flagged  => Clean
          }
          val (newRow, newCol) = stepForward(nextDirection, row, col)
          val newInfectedCount = if (newState == Infected) 1 else 0
          part2(
            Node(nextDirection, newRow, newCol),
            grid.updated((row, col), newState),
            iteration + 1,
            nodesInfected + newInfectedCount
          )
        // An unknown node, but we assume it is uninfected
        case None =>
          val nextDirection    = turnLeft(direction)
          val (newRow, newCol) = stepForward(nextDirection, row, col)
          val newState         = Weakened
          part2(
            Node(nextDirection, newRow, newCol),
            grid.updated((row, col), newState),
            iteration + 1,
            nodesInfected
          )
      }
    }

  private def turnLeft(direction: Direction): Direction = direction match {
    case Up    => Left
    case Right => Up
    case Down  => Right
    case Left  => Down
  }

  private def turnRight(direction: Direction): Direction = direction match {
    case Up    => Right
    case Right => Down
    case Down  => Left
    case Left  => Up
  }

  private def reverse(direction: Direction): Direction = direction match {
    case Up    => Down
    case Right => Left
    case Down  => Up
    case Left  => Right
  }

  private def stepForward(direction: Direction, row: Int, col: Int): (Int, Int) = direction match {
    case Up    => (row - 1, col)
    case Right => (row, col + 1)
    case Down  => (row + 1, col)
    case Left  => (row, col - 1)
  }
}

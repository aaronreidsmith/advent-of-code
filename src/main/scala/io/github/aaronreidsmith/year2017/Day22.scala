package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.*
import io.github.aaronreidsmith.implicits.toGrid

import scala.annotation.tailrec
import scala.io.Source

object Day22 extends Solution {
  type I  = (Node, Grid[State])
  type O1 = Int
  type O2 = Int

  enum State {
    case Clean, Weakened, Flagged, Infected
  }

  object State {
    def apply(char: Char): State = char match {
      case '.' => Clean
      case 'W' => Weakened
      case 'F' => Flagged
      case '#' => Infected
      case _   => throw new IllegalArgumentException
    }
  }

  case class Node(position: Point, direction: Direction)

  override def parseInput(file: Source): (Node, Grid[State]) = {
    val grid   = file.toGrid.view.mapValues(State(_)).toMap.withDefaultValue(State.Clean)
    val middle = grid.keySet.map(_.x).size / 2
    val start  = Node(Point(middle, middle), Direction.North)
    (start, grid)
  }

  override def part1(input: (Node, Grid[State])): Int = {
    val (initialNode, initialGrid) = input

    @tailrec
    def helper(node: Node, grid: Grid[State], iteration: Int = 0, nodesInfected: Int = 0): Int = {
      if (iteration >= 10_000) {
        nodesInfected
      } else {
        val Node(position, direction) = node
        val state                     = grid(position)
        val isInfected                = state == State.Infected
        val nextDirection             = if (isInfected) direction.right else direction.left
        val nextPosition              = position.move(nextDirection)
        val newState                  = if (isInfected) State.Clean else State.Infected
        val newInfectionCount         = if (newState == State.Infected) 1 else 0
        helper(
          Node(nextPosition, nextDirection),
          grid.updated(position, newState),
          iteration + 1,
          nodesInfected + newInfectionCount
        )
      }
    }

    helper(initialNode, initialGrid)
  }

  override def part2(input: (Node, Grid[State])): Int = {
    val (initialNode, initialGrid) = input

    @tailrec
    def helper(node: Node, grid: Grid[State], iteration: Int = 0, nodesInfected: Int = 0): Int = {
      if (iteration >= 10_000_000) {
        nodesInfected
      } else {
        val Node(position, direction) = node
        val state                     = grid(position)
        val nextDirection = state match {
          case State.Clean    => direction.left
          case State.Weakened => direction
          case State.Infected => direction.right
          case State.Flagged  => direction.opposite
        }
        val newState = state match {
          case State.Clean    => State.Weakened
          case State.Weakened => State.Infected
          case State.Infected => State.Flagged
          case State.Flagged  => State.Clean
        }
        val nextPosition      = position.move(nextDirection)
        val newInfectionCount = if (newState == State.Infected) 1 else 0
        helper(
          Node(nextPosition, nextDirection),
          grid.updated(position, newState),
          iteration + 1,
          nodesInfected + newInfectionCount
        )
      }
    }

    helper(initialNode, initialGrid)
  }
}

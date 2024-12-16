package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Direction, Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day15 extends Solution {
  type I  = (String, List[Direction])
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): (String, List[Direction]) = {
    val Array(rawGrid, rawInstructions, _*) = file.mkString.split("\n\n"): @unchecked
    (rawGrid, rawInstructions.toList.flatMap(c => Try(Direction.fromChar(c)).toOption))
  }

  override def part1(input: (String, List[Direction])): Int = {
    val (rawGrid, initialInstructions) = input

    @tailrec
    def helper(state: Grid[Char], instructions: List[Direction]): Int = {
      def robot: Point = state.collectFirst { case (point, char) if char == '@' => point }.get

      @tailrec
      def findBoxesToMove(direction: Direction, acc: List[Point]): List[Point] = {
        val moved = acc.head.move(direction)
        state.get(moved) match {
          case Some('#') | None => Nil
          case Some('.')        => acc
          case Some('O')        => findBoxesToMove(direction, moved :: acc)
          case _                => throw IllegalArgumentException()
        }
      }

      instructions match {
        case Nil =>
          state.foldLeft(0) {
            case (acc, (point, 'O')) => acc + (point.x * 100 + point.y)
            case (acc, _)            => acc
          }
        case head :: tail =>
          val boxesToMove = findBoxesToMove(head, List(robot))
          val updated = boxesToMove.foldLeft(state) { (acc, point) =>
            acc ++ Map(point.move(head) -> state(point), point -> '.')
          }
          helper(updated, tail)
      }
    }

    helper(rawGrid.toGrid, initialInstructions)
  }

  // Adapted from https://github.com/sim642/adventofcode/blob/e6f4e3be68f4838677ba582dd55c833a86b1004a/src/main/scala/eu/sim642/adventofcodelib/GridImplicits.scala
  override def part2(input: (String, List[Direction])): Int = {
    val vertical                = Set(Direction.North, Direction.South)
    val (rawGrid, instructions) = input
    val initialGrid = rawGrid.flatMap {
      case '#' => "##"
      case 'O' => "[]"
      case '.' => ".."
      case '@' => "@."
      case c   => c.toString
    }.toGrid
    val initialRobot = initialGrid.collectFirst { case (p, c) if c == '@' => p }.get

    def move(state: Grid[Char], robot: Point, direction: Direction): (Grid[Char], Point) = {
      def helper(grid: Grid[Char], pos: Point): Option[Grid[Char]] = {
        grid(pos) match {
          case '.' => Some(grid)
          case '#' => None
          case '[' if vertical.contains(direction) =>
            val newPos = pos.move(direction)
            for {
              newGrid  <- helper(grid, newPos)
              newGrid2 <- helper(newGrid, newPos.right)
            } yield {
              newGrid2
                .updated(newPos, '[')
                .updated(newPos.right, ']')
                .updated(pos, '.')
                .updated(pos.right, '.')
            }
          case ']' if vertical.contains(direction) =>
            helper(grid, pos.left)
          case cell @ ('[' | ']') if !vertical.contains(direction) =>
            val newPos = pos.move(direction)
            helper(grid, newPos).map { newGrid =>
              newGrid
                .updated(newPos, cell)
                .updated(pos, '.')
            }
          case _ => throw IllegalArgumentException()
        }
      }

      val newRobot = robot.move(direction)
      helper(state, newRobot).fold((state, robot))((_, newRobot))
    }

    instructions
      .foldLeft((initialGrid.updated(initialRobot, '.'), initialRobot)) {
        case ((grid, robot), direction) => move(grid, robot, direction)
      }
      ._1
      .foldLeft(0) {
        case (acc, (point, '[')) => acc + (point.x * 100 + point.y)
        case (acc, _)            => acc
      }
  }
}

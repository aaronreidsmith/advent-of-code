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
            val char = state(point)
            acc.updated(point.move(head), char).updated(point, '.')
          }
          helper(updated, tail)
      }
    }

    helper(rawGrid.toGrid, initialInstructions)
  }

  override def part2(input: (String, List[Direction])): Int = {
    ???
  }
}

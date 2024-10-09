package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.*
import io.github.aaronreidsmith.extensions.{occurrences, toGrid}

import scala.annotation.tailrec
import scala.io.Source

object Day23 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Char] = file.toGrid
  override def part1(input: Grid[Char]): Int        = solution(input, part1 = true)
  override def part2(input: Grid[Char]): Int        = solution(input, part1 = false)

  private def solution(input: Grid[Char], part1: Boolean): Int = {
    @tailrec
    def helper(
        grid: Grid[Char],
        previousGrid: Grid[Char] = Map.empty[Point, Char],
        directions: Vector[Direction] = Vector(Direction.North, Direction.South, Direction.West, Direction.East),
        iteration: Int = 0
    ): Int = if (part1 && iteration >= 10) {
      val (xs, ys)     = grid.keys.unzip(_.asPair)
      val (minX, maxX) = (xs.min, xs.max)
      val (minY, maxY) = (ys.min, ys.max)

      {
        for {
          x <- minX to maxX
          y <- minY to maxY
          if grid(Point(x, y)) == '.'
        } yield 1
      }.sum
    } else if (!part1 && grid == previousGrid) {
      iteration
    } else {
      // First half of round
      val proposals = grid.collect {
        case (point, '#') =>
          val nEmpty  = grid(point.up) == '.'
          val neEmpty = grid(point.up.right) == '.'
          val eEmpty  = grid(point.right) == '.'
          val seEmpty = grid(point.down.right) == '.'
          val sEmpty  = grid(point.down) == '.'
          val swEmpty = grid(point.down.left) == '.'
          val wEmpty  = grid(point.left) == '.'
          val nwEmpty = grid(point.up.left) == '.'

          if (Seq(nEmpty, neEmpty, eEmpty, seEmpty, sEmpty, swEmpty, wEmpty, nwEmpty).forall(_ == true)) {
            point -> point
          } else {
            directions
              .collectFirst {
                case Direction.North if nEmpty && neEmpty && nwEmpty => point -> point.up
                case Direction.East if eEmpty && neEmpty && seEmpty  => point -> point.right
                case Direction.South if sEmpty && seEmpty && swEmpty => point -> point.down
                case Direction.West if wEmpty && nwEmpty && swEmpty  => point -> point.left
              }
              .getOrElse(point -> point)
          }
      }

      // Second half of round
      val proposalCounts = proposals.values.toSeq.occurrences

      val updated = grid.foldLeft(grid) {
        case (acc, (point, '#')) =>
          val target = proposals(point)
          if (proposalCounts(target) == 1) {
            acc ++ Map(point -> '.', target -> '#')
          } else {
            acc
          }
        case (acc, _) => acc
      }

      helper(updated, grid, directions.tail :+ directions.head, iteration + 1)
    }

    helper(input.withDefaultValue('.'))
  }
}

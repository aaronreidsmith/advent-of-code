package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.implicits._
import io.github.aaronreidsmith.{Direction, East, Grid, North, Point, Solution, South, West}

import scala.annotation.tailrec
import scala.io.Source

object Day23 extends Solution(2022, 23) {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  private implicit class CharOps(char: Char) {
    def isEmpty: Boolean  = char == '.'
    def nonEmpty: Boolean = !isEmpty
  }

  override protected[year2022] def parseInput(file: Source): Grid[Char] = file.toGrid
  override protected[year2022] def part1(input: Grid[Char]): Int        = solution(input, part1 = true)
  override protected[year2022] def part2(input: Grid[Char]): Int        = solution(input, part1 = false)

  private def solution(input: Grid[Char], part1: Boolean): Int = {
    @tailrec
    def helper(
        grid: Grid[Char],
        previousGrid: Grid[Char] = Map.empty[Point, Char],
        directions: Vector[Direction] = Vector(North, South, West, East),
        iteration: Int = 0
    ): Int = if (part1 && iteration >= 10) {
      val (xs, ys)     = grid.keys.unzip
      val (minX, maxX) = (xs.min, xs.max)
      val (minY, maxY) = (ys.min, ys.max)

      {
        for {
          x <- minX to maxX
          y <- minY to maxY
          if grid(Point(x, y)).isEmpty
        } yield 1
      }.sum
    } else if (!part1 && grid == previousGrid) {
      iteration
    } else {
      // First half of round
      val proposals = grid.collect {
        case (point, char) if char.nonEmpty =>
          val n  = grid(point.up)
          val ne = grid(point.up.right)
          val e  = grid(point.right)
          val se = grid(point.down.right)
          val s  = grid(point.down)
          val sw = grid(point.down.left)
          val w  = grid(point.left)
          val nw = grid(point.up.left)

          if (Seq(n, ne, e, se, s, sw, w, nw).forall(_.isEmpty)) {
            point -> point
          } else {
            directions
              .collectFirst {
                case North if n.isEmpty && ne.isEmpty && nw.isEmpty => point -> point.up
                case East if e.isEmpty && ne.isEmpty && se.isEmpty  => point -> point.right
                case South if s.isEmpty && se.isEmpty && sw.isEmpty => point -> point.down
                case West if w.isEmpty && nw.isEmpty && sw.isEmpty  => point -> point.left
              }
              .getOrElse(point -> point)
          }
      }

      // Second half of round
      val proposalCounts = proposals.values.toSeq.occurrences

      val updated = grid.foldLeft(grid) {
        case (acc, (point, char)) if char.nonEmpty =>
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

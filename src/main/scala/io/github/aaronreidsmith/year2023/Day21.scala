package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.implicits.{mod, toGrid}
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/keaiiq7
@Slow(part2 = true)
object Day21 extends Solution {
  type I  = (Point, Grid[Char])
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): (Point, Grid[Char]) = {
    val grid  = file.toGrid
    val start = grid.collectFirst { case (point, char) if char == 'S' => point }.get
    (start, grid)
  }

  override def part1(input: (Point, Grid[Char])): Int = {
    val (start, grid) = input

    @tailrec
    def helper(current: Set[Point], iteration: Int = 0): Int = if (iteration >= 64) {
      current.size
    } else {
      val moved = current.flatMap { point =>
        point.immediateNeighbors.filter(neighbor => grid.contains(neighbor) && grid(neighbor) != '#')
      }
      helper(moved, iteration + 1)
    }

    helper(Set(start))
  }

  override def part2(input: (Point, Grid[Char])): Long = {
    val (start, grid) = input
    val goal          = 26501365
    val gridSize      = grid.keys.max.x + 1

    @tailrec
    def helper(current: Set[Point], points: List[Long] = Nil, iteration: Int = 1): List[Long] = {
      if (points.size >= 3) {
        points
      } else {
        val moved = current.flatMap { point =>
          point.immediateNeighbors.filter { neighbor =>
            val normalizedNeighbor = Point(neighbor.x.mod(gridSize), neighbor.y.mod(gridSize))
            grid.contains(normalizedNeighbor) && grid(normalizedNeighbor) != '#'
          }
        }
        val newPoints = if (iteration % gridSize == goal % gridSize) points :+ moved.size.toLong else points
        helper(moved, newPoints, iteration + 1)
      }
    }

    val List(a0, a1, a2, _*) = helper(Set(start)): @unchecked
    val n                    = goal.toLong / gridSize.toLong
    val b0                   = a0
    val b1                   = a1 - a0
    val b2                   = a2 - a1
    b0 + b1 * n + (n * (n - 2) / 2) * (b2 - b1)
  }
}

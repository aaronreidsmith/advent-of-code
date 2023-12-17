package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.implicits.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

// Adapted from https://www.reddit.com/r/adventofcode/comments/18k9ne5/comment/kdq86mr
object Day17 extends Solution {
  type I  = Grid[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Int] = file.toGrid.view.mapValues(_.asDigit).toMap

  override def part1(input: Grid[Int]): Int = solution(input, 1, 3)

  override def part2(input: Grid[Int]): Int = solution(input, 4, 10)

  private def solution(grid: Grid[Int], minStep: Int, maxStep: Int): Int = {
    val end     = grid.keys.max
    val queue   = mutable.PriorityQueue((0, 0, 0, 0, 0))(implicitly[Ordering[(Int, Int, Int, Int, Int)]].reverse)
    val seen    = mutable.Set.empty[(Int, Int, Int, Int)]
    var minHeat = 0
    breakable {
      while (queue.nonEmpty) {
        val (heat, x, y, px, py) = queue.dequeue()
        if (Point(x, y) == end) {
          minHeat = heat
          break()
        }
        if (seen.add((x, y, px, py))) {
          Set((1, 0), (0, 1), (-1, 0), (0, -1)).diff(Set((px, py), (-px, -py))).foreach { (dx, dy) =>
            var (a, b, h) = (x, y, heat)
            (1 to maxStep).foreach { i =>
              a += dx
              b += dy
              grid.get(Point(a, b)).foreach { heatLoss =>
                h += heatLoss
                if (i >= minStep) {
                  queue.enqueue((h, a, b, dx, dy))
                }
              }
            }
          }
        }
      }
    }
    minHeat
  }
}

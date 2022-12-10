package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith._
import io.github.aaronreidsmith.implicits._

import scala.annotation.tailrec
import scala.io.Source

object Day18 extends Solution(2015, 18) {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  private val part1Iterations = if (isTest) 4 else 100
  private val part2Iterations = if (isTest) 5 else 100

  override protected[year2015] def parseInput(file: Source): Grid[Char] = file.toGrid
  override protected[year2015] def part1(grid: Grid[Char]): Int         = solution(grid, part1Iterations, Seq(), 0)
  override protected[year2015] def part2(grid: Grid[Char]): Int = {
    val (rows, cols) = grid.keys.unzip
    val corners = for {
      row <- Seq(rows.min, rows.max)
      col <- Seq(cols.min, cols.max)
    } yield Point(row, col)
    val cornersOn = grid ++ corners.map(_ -> '#')
    solution(cornersOn, part2Iterations, corners, 0)
  }

  @tailrec
  private def solution(grid: Grid[Char], iterations: Int, alwaysOn: Seq[Point], iteration: Int): Int =
    if (iteration >= iterations) {
      grid.values.count(_ == '#')
    } else {
      val updated = grid.map {
        case (point, char) =>
          if (alwaysOn.contains(point)) {
            point -> '#'
          } else {
            val isOn        = char == '#'
            val neighborsOn = point.neighbors.count(position => grid.get(position).fold(false)(_ == '#'))
            val newState = isOn match {
              case true if neighborsOn != 2 && neighborsOn != 3 => '.'
              case true                                         => '#'
              case false if neighborsOn == 3                    => '#'
              case _                                            => '.'
            }
            point -> newState
          }
      }
      solution(updated, iterations, alwaysOn, iteration + 1)
    }
}

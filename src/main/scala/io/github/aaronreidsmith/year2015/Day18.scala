package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith._
import io.github.aaronreidsmith.implicits._

import scala.annotation.tailrec

object Day18 {
  def main(args: Array[String]): Unit = {
    val grid = using("2015/day18.txt")(_.toCharGrid)
    println(s"Part 1: ${part1(grid)}")
    println(s"Part 2: ${part2(grid)}")
  }

  private[year2015] def part1(grid: Grid[Char], iterations: Int = 100): Int = solution(grid, iterations, Seq(), 0)
  private[year2015] def part2(grid: Grid[Char], iterations: Int = 100): Int = {
    val (rows, cols) = grid.keys.unzip
    val corners = for {
      row <- Seq(rows.min, rows.max)
      col <- Seq(cols.min, cols.max)
    } yield Point(row, col)
    val cornersOn = grid ++ corners.map(_ -> '#')
    solution(cornersOn, iterations, corners, 0)
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

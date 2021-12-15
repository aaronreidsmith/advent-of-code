package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day15 {
  private implicit class GridOps(grid: Map[(Int, Int), Int]) {
    def neighbors(point: (Int, Int)): Seq[(Int, Int)] = {
      val (row, col) = point
      Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)).filter(grid.contains)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Using
      .resource(Source.fromResource("2021/day15.txt")) { file =>
        for {
          (line, row) <- file.getLines().toList.zipWithIndex
          (risk, col) <- line.zipWithIndex
        } yield (row, col) -> risk.asDigit
      }
      .toMap
    val expandedInput = {
      val (maxRow, maxCol) = input.keys.maxBy { case (row, col) => row + col }
      val (height, width)  = (maxRow + 1, maxCol + 1)
      Vector
        .tabulate(5, 5) { (x, y) =>
          input.map {
            case ((row, col), risk) => (x * width + row, y * height + col) -> (1 + (risk - 1 + x + y) % 9)
          }
        }
        .flatten
        .reduceLeft(_ ++ _)
    }

    println(s"Part 1: ${solution(input)}")
    println(s"Part 2: ${solution(expandedInput)}")
  }

  private def solution(grid: Map[(Int, Int), Int]): Int = {
    val start = (0, 0)
    val end   = grid.keys.maxBy { case (row, col) => row + col }

    @tailrec
    def helper(toCheck: Set[(Int, Int)], risk: Map[(Int, Int), Int]): Int = {
      val currentPoint = toCheck.minBy(risk)
      if (currentPoint == end) {
        risk(end)
      } else {
        val (updatedToCheck, updatedRisk) = grid
          .neighbors(currentPoint)
          .foldLeft((toCheck - currentPoint, risk)) {
            case ((toCheckAcc, riskAcc), position)
                if !risk.contains(position) || risk(currentPoint) + grid(position) < risk(position) =>
              (toCheckAcc + position, riskAcc.updated(position, risk(currentPoint) + grid(position)))
            case (accs, _) => accs
          }
        helper(updatedToCheck, updatedRisk)
      }
    }

    helper(Set(start), Map(start -> 0))
  }
}

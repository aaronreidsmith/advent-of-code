package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

@Slow(part2 = true)
object Day15 extends Solution {
  type I  = Grid[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Int] = file.toGrid.view.mapValues(_.asDigit).toMap
  override def part1(input: Grid[Int]): Int        = solution(input)
  override def part2(input: Grid[Int]): Int = {
    val (maxRow, maxCol) = input.keys.maxBy(point => point.x + point.y).asPair
    val (height, width)  = (maxRow + 1, maxCol + 1)
    val expandedInput = Vector
      .tabulate(5, 5) { (x, y) =>
        input.map {
          case (position, risk) =>
            Point(x * width + position.x, y * height + position.y) -> (1 + (risk - 1 + x + y) % 9)
        }
      }
      .flatten
      .reduceLeft(_ ++ _)
    solution(expandedInput)
  }

  private def solution(grid: Grid[Int]): Int = {
    val start = Point.zero
    val end   = grid.keys.maxBy(point => point.x + point.y)

    @tailrec
    def helper(toCheck: Set[Point], risk: Grid[Int]): Int = {
      val currentPoint = toCheck.minBy(risk)
      if (currentPoint == end) {
        risk(end)
      } else {
        val (updatedToCheck, updatedRisk) = currentPoint.immediateNeighbors
          .filter(grid.contains)
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

package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day10 extends Solution {
  type I  = List[Point]
  type O1 = Int
  type O2 = String

  case class Point(x: Int, y: Int, dx: Int, dy: Int) {
    def next: Point = this.copy(x = x + dx, y = y + dy)
  }

  override def parseInput(file: Source): List[Point] = {
    val entry = "^position=<(.*),(.*)> velocity=<(.*),(.*)>$".r
    file.getLines().toList.collect {
      case entry(x, y, dx, dy) => Point(x.trim.toInt, y.trim.toInt, dx.trim.toInt, dy.trim.toInt)
    }
  }

  override def part1(input: List[Point]): Int    = solution(input)._1
  override def part2(input: List[Point]): String = solution(input)._2

  // Both parts require same traversal, so might as well only do it once
  private var answer = (0, "")
  private var solved = false
  private def solution(points: List[Point]): (Int, String) = {
    if (!solved) {
      var running       = true
      var counter       = 1
      val mutablePoints = points.toArray
      while (running) {
        mutablePoints.mapInPlace(_.next)
        val yBound = mutablePoints.map(_.y).max - mutablePoints.map(_.y).min
        if (yBound < 12) {
          running = false
        }
        counter += 1
      }

      val grid = new StringBuilder("\n")
      // Find boundaries
      val (xMin, xMax, yMin, yMax) = mutablePoints.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)) {
        case ((currXMin, currXMax, currYMin, currYMax), point) =>
          (currXMin.min(point.x), currXMax.max(point.x), currYMin.min(point.y), currYMax.max(point.y))
        case (acc, _) => acc
      }

      (yMin to yMax).foreach { y =>
        (xMin to xMax).foreach { x =>
          val char = if (mutablePoints.exists(point => point.x == x && point.y == y)) '#' else ' '
          grid.append(char)
        }
        grid.append("\n")
      }
      answer = (counter, grid.result())
      solved = true
    }

    answer
  }
}

package io.github.aaronreidsmith.year2018

import scala.io.Source

object Day10 {
  private val entry = "^position=<(.*),(.*)> velocity=<(.*),(.*)>$".r("x", "y", "xDelta", "yDelta")

  private case class Point(x: Int, y: Int, xDelta: Int, yDelta: Int) {
    def next: Point = Point(x + xDelta, y + yDelta, xDelta, yDelta)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2018/day10.txt")
    val points = input.getLines().foldLeft(List.empty[Point]) {
      case (acc, entry(x, y, xDelta, yDelta)) =>
        acc :+ Point(x.trim.toInt, y.trim.toInt, xDelta.trim.toInt, yDelta.trim.toInt)
    }
    input.close()

    printGrid(points)
  }

  private def printGrid(points: List[Point]): Unit = {
    // Find boundaries
    val (xMin, xMax, yMin, yMax) =
      points.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE)) {
        case ((currXMin, currXMax, currYMin, currYMax), point) =>
          val nextXMin = math.min(currXMin, point.x)
          val nextXMax = math.min(currXMax, point.x)
          val nextYMin = math.min(currYMin, point.y)
          val nextYMax = math.min(currYMax, point.y)
          (nextXMin, nextXMax, nextYMin, nextYMax)
      }

    // Normalize everything to 0-based
    val (xNormalized, newXMax) =
      if (xMin < 0) (points.map(point => point.copy(x = point.x - xMin)), xMax - xMin) else (points, xMax)
    val (normalized, newYMax) =
      if (yMin < 0) (points.map(point => point.copy(y = point.y - yMin)), yMax - yMin) else (xNormalized, yMax)

    val grid = Array.fill(newXMax)(Array.fill(newYMax)('.'))

    normalized.foreach { point =>
      grid(point.x)(point.y) = '#'
    }

    println(grid.map(_.mkString).mkString("\n"))
  }
}

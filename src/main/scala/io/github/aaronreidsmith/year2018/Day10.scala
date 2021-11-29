package io.github.aaronreidsmith.year2018

import scala.io.Source

object Day10 {
  private val entry = "^position=<(.*),(.*)> velocity=<(.*),(.*)>$".r

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

    solution(points)
  }

  private def solution(points: List[Point]): Unit = {
    var running       = true
    var counter       = 1
    var mutablePoints = points
    while (running) {
      mutablePoints = mutablePoints.map(_.next)
      val yBound = mutablePoints.map(_.y).max - mutablePoints.map(_.y).min
      if (yBound < 12) {
        println(counter)
        printGrid(mutablePoints)
        running = false
      }
      counter += 1
    }
  }

  private def printGrid(points: List[Point]): Unit = {
    // Find boundaries
    val (xMin, xMax, yMin, yMax) =
      points.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE)) {
        case ((currXMin, currXMax, currYMin, currYMax), point) =>
          val nextXMin = math.min(currXMin, point.x)
          val nextXMax = math.max(currXMax, point.x)
          val nextYMin = math.min(currYMin, point.y)
          val nextYMax = math.max(currYMax, point.y)
          (nextXMin, nextXMax, nextYMin, nextYMax)
      }

    (yMin to yMax).foreach { y =>
      (xMin to xMax).foreach { x =>
        val char = if (points.exists(point => point.x == x && point.y == y)) '#' else ' '
        print(char)
      }
      println()
    }
  }
}

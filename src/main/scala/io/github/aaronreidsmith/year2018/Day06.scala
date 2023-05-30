package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

// TODO: Adapted from Raku solution
object Day06 extends Solution {
  type I  = List[Coordinate]
  type O1 = Int
  type O2 = Int

  case class Coordinate(position: Point, var area: Int = 0, var edge: Boolean = false)

  override def parseInput(file: Source): List[Coordinate] = file.getLines().toList.map { line =>
    val Array(x, y, _*) = line.split(", "): @unchecked
    Coordinate(Point(x.toInt, y.toInt))
  }

  override def part1(coordinates: List[Coordinate]): Int = solution(coordinates)._1
  override def part2(coordinates: List[Coordinate]): Int = solution(coordinates)._2

  private var answer = (0, 0)
  private var solved = false
  private def solution(initialCoordinates: List[Coordinate]): (Int, Int) = {
    if (!solved) {
      val coordinates       = initialCoordinates.toBuffer
      val maxRegionDistance = if (isTest) 32 else 10_000

      val (xs, ys) = initialCoordinates.unzip(coordinate => coordinate.position.asPair)
      val xMin     = xs.min
      val xMax     = xs.max
      val yMin     = ys.min
      val yMax     = ys.max

      var part2Area = 0

      for {
        y <- yMin to yMax
        x <- xMin to xMax
        point = Point(x, y)
      } {
        val distPairs   = coordinates.map(coordinate => (coordinate.position.manhattanDistance(point), coordinate))
        val distances   = distPairs.map(_._1)
        val minDistance = distances.min

        if (distances.sum < maxRegionDistance) {
          part2Area += 1
        }

        val minimums = distPairs.filter(_._1 == minDistance)
        if (minimums.size == 1) {
          val coord = minimums.head._2
          coord.area += 1
          coord.edge = x == xMin || x == xMax || y == yMin || y == yMax
        }
      }

      val part1 = coordinates.filter(coord => !coord.edge).maxBy(_.area).area
      val part2 = part2Area

      answer = (part1, part2)
      solved = true
    }

    answer
  }
}

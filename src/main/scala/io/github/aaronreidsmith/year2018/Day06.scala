package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using

import scala.io.Source
import scala.language.implicitConversions

// TODO: Adapted from Raku solution
object Day06 {
  // So we can unzip into xs and ys
  private implicit def coordinate2Tuple(coordinate: Coordinate): (Int, Int) = (coordinate.x, coordinate.y)

  private[year2018] case class Coordinate(x: Int, y: Int, var area: Int = 0, var edge: Boolean = false) {
    def distance(that: (Int, Int)): Int = {
      val (thatX, thatY) = that
      (x - thatX).abs + (y - thatY).abs
    }
  }

  def main(args: Array[String]): Unit = {
    val coordinates = using("2018/day06.txt")(parseInput)
    println(s"Part 1: ${part1(coordinates)}")
    println(s"Part 2: ${part2(coordinates)}")
  }

  private[year2018] def parseInput(file: Source): List[Coordinate] = file.getLines().toList.map { line =>
    val Array(x, y) = line.split(", ", 2)
    Coordinate(x.toInt, y.toInt)
  }

  private[year2018] def part1(coordinates: List[Coordinate]): Int = solution(coordinates)._1
  private[year2018] def part2(coordinates: List[Coordinate]): Int = solution(coordinates)._2

  // Using a lazy val so we don't call this twice
  private lazy val solution: List[Coordinate] => (Int, Int) = (initialCoordinates: List[Coordinate]) => {
    val coordinates = initialCoordinates.toBuffer

    val (xs, ys) = initialCoordinates.unzip
    val xMin = xs.min
    val xMax = xs.max
    val yMin = ys.min
    val yMax = ys.max

    var part2Area = 0

    for {
      y <- yMin to yMax
      x <- xMin to xMax
    } {
      val distPairs = coordinates.map(coordinate => (coordinate.distance((x, y)), coordinate))
      val distances = distPairs.map(_._1)
      val minDistance = distances.min

      if (distances.sum < 10_000) {
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

    (part1, part2)
  }
}

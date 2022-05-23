package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Point, using}

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/e8m1z3/comment/faeb25d
object Day10 {
  private implicit class PointOps(point: Point) {
    def visibleFrom(points: List[Point]): Set[Point] = {
      @tailrec
      def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

      points.map { p =>
        val (dx, dy) = (p.x - point.x, p.y - point.y)
        val g        = gcd(dx, dy).abs
        Point(dx / g, dy / g)
      }.toSet
    }
  }

  def main(args: Array[String]): Unit = {
    val asteroids = using("2019/day10.txt")(parseInput)
    println(s"Part 1: ${part1(asteroids)}")
    println(s"Part 2: ${part2(asteroids)}")
  }

  private[year2019] def parseInput(file: Source): Set[Point] = {
    for {
      (line, row) <- file.getLines().zipWithIndex
      (char, col) <- line.zipWithIndex
      if char != '.'
    } yield Point(row, col)
  }.toSet

  private[year2019] def part1(asteroids: Set[Point]): Int = findStationCoordinates(asteroids)._2.size

  private[year2019] def part2(asteroids: Set[Point]): Int = {
    val (station, targets) = findStationCoordinates(asteroids)
    val target = station + targets.toIndexedSeq
      .map(asteroid => (math.atan2(asteroid.y, asteroid.x), asteroid))
      .sortBy { case (angle, Point(x, y)) => (-angle, -x, -y) }
      .apply(199)
      ._2
    target.y * 100 + target.x
  }

  private def findStationCoordinates(asteroids: Set[Point]): (Point, Set[Point]) =
    asteroids.toSeq.foldLeft((Point.zero, Set.empty[Point])) {
      case ((currentStation, currentTargets), asteroid) =>
        val others  = asteroids - asteroid
        val visible = asteroid.visibleFrom(others.toList)
        if (visible.size > currentTargets.size) {
          (asteroid, visible)
        } else {
          (currentStation, currentTargets)
        }
      case (acc, _) => acc
    }
}

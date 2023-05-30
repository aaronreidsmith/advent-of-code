package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Point, Solution}
import org.apache.commons.math3.util.ArithmeticUtils

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/e8m1z3/comment/faeb25d
object Day10 extends Solution {
  type I  = Set[Point]
  type O1 = Int
  type O2 = Int

  extension (point: Point) {
    def visibleFrom(points: List[Point]): Set[Point] = points.foldLeft(Set.empty[Point]) { (acc, p) =>
      val Point(dx, dy) = p - point
      val gcd           = ArithmeticUtils.gcd(dx, dy)
      acc + Point(dx / gcd, dy / gcd)
    }
  }

  override def parseInput(file: Source): Set[Point] = {
    for {
      (line, row) <- file.getLines().zipWithIndex
      (char, col) <- line.zipWithIndex
      if char != '.'
    } yield Point(row, col)
  }.toSet

  override def part1(asteroids: Set[Point]): Int = findStationCoordinates(asteroids)._2.size

  override def part2(asteroids: Set[Point]): Int = {
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
    }
}

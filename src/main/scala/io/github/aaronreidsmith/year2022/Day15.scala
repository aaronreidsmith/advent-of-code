package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, Solution, using}

import scala.io.Source

object Day15 extends Solution(2022, 15) {
  type I  = List[Sensor]
  type O1 = Int
  type O2 = Long

  private[year2022] case class Sensor(position: Point, closestBeacon: Point) {
    def distance: Int = position.manhattanDistance(closestBeacon)
  }

  private case class Interval(min: Int, max: Int) {
    def size: Int                          = max - min + 1
    def merge(other: Interval): Interval   = Interval(min.min(other.min), max.max(other.max))
    def touching(other: Interval): Boolean = if (other.min < min) min - other.max <= 1 else other.min - max <= 1
  }

  override protected[year2022] def parseInput(file: Source): List[Sensor] = {
    val entry = """^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$""".r
    file.getLines().toList.collect {
      case entry(sX, sY, bX, bY) => Sensor(Point(sX.toInt, sY.toInt), Point(bX.toInt, bY.toInt))
    }
  }

  override protected[year2022] def part1(input: List[Sensor]): Int = {
    val row     = if (isTest) 10 else 2000000
    val empty   = findEmpty(input, row).foldLeft(0)(_ + _.size)
    val beacons = input.collect { case sensor if sensor.closestBeacon.y == row => sensor.closestBeacon }.distinct.size
    empty - beacons
  }

  override protected[year2022] def part2(input: List[Sensor]): Long = {
    Iterator
      .from(0)
      .map(findEmpty(input, _))
      .zipWithIndex
      .collectFirst {
        case (List(first, second), row) => 4000000L * (first.max.min(second.max) + 1) + row
      }
      .get
  }

  private def findEmpty(sensors: List[Sensor], targetRow: Int): List[Interval] = {
    sensors.foldLeft(List.empty[Interval]) { (acc, sensor) =>
      val extra = sensor.distance - (sensor.position.y - targetRow).abs
      if (extra < 0) {
        acc
      } else {
        val next      = Interval(sensor.position.x - extra, sensor.position.x + extra)
        val (in, out) = acc.partition(_.touching(next))
        in.foldLeft(next)(_.merge(_)) :: out
      }
    }
  }
}

package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.*

import scala.io.Source

object Day02 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  // Only want to compile these once
  private val forward = """^forward (\d+)$""".r
  private val down    = """^down (\d+)$""".r
  private val up      = """^up (\d+)$""".r

  override def parseInput(file: Source): List[String] = file.getLines().toList

  override def part1(input: List[String]): Int = {
    val finalPos = input.foldLeft(Point.zero) {
      case (point, forward(x)) => point.move(Direction.East, x.toInt)
      case (point, down(x))    => point.move(Direction.South, x.toInt)
      case (point, up(x))      => point.move(Direction.North, x.toInt)
      case (point, _)          => point
    }
    finalPos.x * finalPos.y
  }

  override def part2(input: List[String]): Int = {
    val (finalPos, _) = input.foldLeft((Point.zero, 0)) {
      case ((point, aim), forward(x)) => (Point(point.x + aim * x.toInt, point.y + x.toInt), aim)
      case ((point, aim), down(x))    => (point, aim + x.toInt)
      case ((point, aim), up(x))      => (point, aim - x.toInt)
      case (acc, _)                   => acc
    }
    finalPos.x * finalPos.y
  }
}

package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Direction, Point, Solution}

import scala.io.Source

// Adapted from https://github.com/mdekaste/AdventOfCode2023/blob/9e3426959bd66980f6cc81ed82fc60dc29de3cac/src/main/kotlin/day10/Day10.kt
object Day10 extends Solution {
  type I  = List[Point]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[Point] = {
    var startPoint = Point.zero
    val grid = file.toGrid.map { (point, char) =>
      val neighbors = char match {
        case 'L' => List(Direction.North, Direction.East)
        case '|' => List(Direction.North, Direction.South)
        case 'J' => List(Direction.North, Direction.West)
        case 'F' => List(Direction.East, Direction.South)
        case '-' => List(Direction.East, Direction.West)
        case '7' => List(Direction.South, Direction.West)
        case 'S' =>
          startPoint = point
          Direction.values.toList
        case _ => Nil
      }
      point -> neighbors.map(point.move(_))
    }

    // Have to convert to a val for pattern matching
    val start     = startPoint
    val firstMove = grid(start).find(from => grid(from).contains(start)).get
    Iterator
      .iterate((start, firstMove)) { (from, to) =>
        to match {
          case `start` => null
          case _       => (to, grid(to).filterNot(_ == from).head)
        }
      }
      .takeWhile(Option(_).isDefined)
      .map(_._1)
      .toList
  }

  override def part1(input: List[Point]): Int = input.size / 2

  override def part2(input: List[Point]): Int = input
    .appended(input.head)
    .sliding(2)
    .foldLeft(0) {
      case (acc, List(a, b)) => acc + (b.x - a.x) * a.y
      case (acc, _)          => acc
    }
    .abs - part1(input) + 1
}

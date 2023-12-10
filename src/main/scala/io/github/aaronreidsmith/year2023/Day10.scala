package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

// Adapted from https://github.com/mdekaste/AdventOfCode2023/blob/9e3426959bd66980f6cc81ed82fc60dc29de3cac/src/main/kotlin/day10/Day10.kt
object Day10 extends Solution {
  type I  = List[(Int, Int)]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[(Int, Int)] = {
    val north = (-1, 0)
    val east  = (0, 1)
    val south = (1, 0)
    val west  = (0, -1)

    var startPoint: (Int, Int) | Null = null
    val grid = file
      .getLines()
      .zipWithIndex
      .flatMap { (line, y) =>
        line.zipWithIndex.map { (char, x) =>
          val directions = char match {
            case 'L' => List(north, east)
            case '|' => List(north, south)
            case 'J' => List(north, west)
            case 'F' => List(east, south)
            case '-' => List(east, west)
            case '7' => List(south, west)
            case 'S' =>
              startPoint = (y, x)
              List(north, east, south, west)
            case _ => Nil
          }
          ((y, x), directions.map((y2, x2) => (y2 + y, x2 + x)))
        }
      }
      .toMap

    // Have to convert to a val for pattern matching
    val start     = startPoint.nn
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

  override def part1(input: List[(Int, Int)]): Int = input.size / 2

  override def part2(input: List[(Int, Int)]): Int = input
    .appended(input.head)
    .sliding(2)
    .foldLeft(0) {
      case (acc, List((y1, x1), (_, x2))) => acc + (x2 - x1) * y1
      case (acc, _)                       => acc
    }
    .abs - part1(input) + 1
}

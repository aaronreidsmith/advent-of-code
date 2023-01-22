package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source
import scala.util.Using

object Day05 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  private implicit class ListOps(list: List[String]) {
    private val entry = """^(\d+),(\d+) -> (\d+),(\d+)$""".r

    def toVents(includeDiagonals: Boolean): Map[Point, Int] = list.foldLeft(Map.empty[Point, Int].withDefaultValue(0)) {
      case (acc, entry(x1Str, y1Str, x2Str, y2Str)) =>
        val x1 = x1Str.toInt
        val y1 = y1Str.toInt
        val x2 = x2Str.toInt
        val y2 = y2Str.toInt

        if (x1 == x2) { // Horizontal
          val range = if (y1 < y2) y1 to y2 else y1 to y2 by -1
          range.foldLeft(acc) { (innerAcc, y) =>
            val position = Point(x1, y)
            val old      = innerAcc(position)
            innerAcc.updated(position, old + 1)
          }
        } else if (y1 == y2) { // Vertical
          val range = if (x1 < x2) x1 to x2 else x1 to x2 by -1
          range.foldLeft(acc) { (innerAcc, x) =>
            val position = Point(x, y1)
            val old      = innerAcc(position)
            innerAcc.updated(position, old + 1)
          }
        } else if (includeDiagonals) { // Diagonals
          val xRange = if (x1 < x2) x1 to x2 else x1 to x2 by -1
          val yRange = if (y1 < y2) y1 to y2 else y1 to y2 by -1
          xRange.zip(yRange).foldLeft(acc) {
            case (innerAcc, (x, y)) =>
              val position = Point(x, y)
              val old      = innerAcc(position)
              innerAcc.updated(position, old + 1)
            case (innerAcc, _) => innerAcc
          }
        } else {
          acc
        }
      case (acc, _) => acc
    }
  }

  override def parseInput(file: Source): List[String] = file.getLines().toList
  override def part1(input: List[String]): Int        = input.toVents(false).values.count(_ > 1)
  override def part2(input: List[String]): Int        = input.toVents(true).values.count(_ > 1)
}

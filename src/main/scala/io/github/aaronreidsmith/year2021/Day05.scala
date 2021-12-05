package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day05 {
  private val entry = "^(\\d+),(\\d+) -> (\\d+),(\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day05.txt"))(_.getLines().toList)

    val horizontalAndVertical = input
      .foldLeft(Map.empty[(Int, Int), Int].withDefaultValue(0)) {
        // Horizontal
        case (acc, entry(x1, y1, x2, y2)) if x1 == x2 =>
          val xCoordinate = x1.toInt
          val y1Int       = y1.toInt
          val y2Int       = y2.toInt
          val range       = if (y1Int < y2Int) y1Int to y2Int else y1Int to y2Int by -1
          range.foldLeft(acc) { (innerAcc, y) =>
            val position = (xCoordinate, y)
            val old      = innerAcc(position)
            innerAcc.updated(position, old + 1)
          }
        // Vertical
        case (acc, entry(x1, y1, x2, y2)) if y1 == y2 =>
          val yCoordinate = y1.toInt
          val x1Int       = x1.toInt
          val x2Int       = x2.toInt
          val range       = if (x1Int < x2Int) x1Int to x2Int else x1Int to x2Int by -1
          range.foldLeft(acc) { (innerAcc, x) =>
            val position = (x, yCoordinate)
            val old      = innerAcc(position)
            innerAcc.updated(position, old + 1)
          }
        // Diagonal (ignored)
        case (acc, _) => acc
      }

    val part1 = horizontalAndVertical.values.count(_ > 1)
    println(s"Part 1: $part1")

    val part2 = input
      .foldLeft(horizontalAndVertical) {
        // Diagonal
        case (acc, entry(x1, y1, x2, y2)) if x1 != x2 && y1 != y2 =>
          val x1Int  = x1.toInt
          val x2Int  = x2.toInt
          val y1Int  = y1.toInt
          val y2Int  = y2.toInt
          val xRange = if (x1Int < x2Int) x1Int to x2Int else x1Int to x2Int by -1
          val yRange = if (y1Int < y2Int) y1Int to y2Int else y1Int to y2Int by -1
          xRange.zip(yRange).foldLeft(acc) { (innerAcc, position) =>
            val old = innerAcc(position)
            innerAcc.updated(position, old + 1)
          }
        // Horizontal or vertical (already dealt with)
        case (acc, _) => acc
      }
      .values
      .count(_ > 1)
    println(s"Part 2: $part2")
  }
}

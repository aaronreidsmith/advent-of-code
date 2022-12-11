package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{Point, Solution, using}

import scala.io.Source

object Day03 extends Solution(2018, 3) {
  type I  = List[Square]
  type O1 = Int
  type O2 = Int

  private[year2018] case class Square(id: Int, leftIndent: Int, topIndent: Int, width: Int, height: Int)

  override protected[year2018] def parseInput(file: Source): List[Square] = {
    val square = "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".r
    file.getLines().toList.collect {
      case square(id, leftIndent, topIndent, width, height) =>
        Square(id.toInt, leftIndent.toInt, topIndent.toInt, width.toInt, height.toInt)
    }
  }

  override protected[year2018] def part1(squares: List[Square]): Int = getOverlaps(squares).size

  override protected[year2018] def part2(squares: List[Square]): Int = {
    val ids        = squares.map(_.id).toSet
    val overlapSet = getOverlaps(squares).reduceLeft(_ ++ _)
    ids.diff(overlapSet).head
  }

  private def getOverlaps(squares: List[Square]): List[Set[Int]] = squares
    .foldLeft(Map.empty[Point, Set[Int]]) { (acc, square) =>
      val xStart = square.leftIndent
      val xEnd   = xStart + square.width - 1
      val yStart = square.topIndent
      val yEnd   = yStart + square.height - 1

      val points = for {
        x <- xStart to xEnd
        y <- yStart to yEnd
      } yield Point(x, y)

      points.foldLeft(acc) { (innerAcc, point) =>
        val existing = innerAcc.getOrElse(point, Set())
        innerAcc.updated(point, existing + square.id)
      }
    }
    .collect { case (_, overlaps) if overlaps.size > 1 => overlaps }
    .toList
}

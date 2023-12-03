package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.implicits.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.collection.mutable
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/189m3qw/comment/kbswe8o
object Day03 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = {
    getParts(input).foldLeft(0)(_ + _._1)
  }

  override def part2(input: Grid[Char]): Int = {
    val stars = input.toList.collect { case (pos, char) if char == '*' => pos }
    val parts = getParts(input)
    stars.foldLeft(0) { (acc, star) =>
      val partsAdjacentToStars = parts.foldLeft(List.empty[Int]) {
        case (partsAcc, (num, positions)) =>
          if (positions.toSet.intersect(star.neighbors.toSet).nonEmpty) {
            num :: partsAcc
          } else {
            partsAcc
          }
      }

      if (partsAdjacentToStars.length == 2) {
        acc + partsAdjacentToStars.product
      } else {
        acc
      }
    }
  }

  private def getParts(input: Grid[Char]): List[(Int, List[Point])] = {
    def isSymbol(point: Point): Boolean = {
      val char = input.getOrElse(point, '.')
      char != '.' && !char.isDigit
    }

    // TODO: Make more functional
    val numbers = mutable.ListBuffer.empty[(Int, List[Point])]
    var acc = 0
    val points = mutable.ListBuffer.empty[Point]
    input.toSeq.sortBy(_._1).foreach { (pos, char) =>
      if (char.isDigit) {
        points.append(pos)
        acc = 10 * acc + char.asDigit
      } else {
        numbers.append((acc, points.toList))
        acc = 0
        points.clear()
      }
    }

    numbers.toList.filter((_, positions) => positions.flatMap(_.neighbors).exists(isSymbol))
  }
}

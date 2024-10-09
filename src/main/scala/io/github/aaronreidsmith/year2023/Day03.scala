package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/189m3qw/comment/kbswe8o
object Day03 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  private case class Part(number: Int, positions: Vector[Point])

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = {
    getParts(input).foldLeft(0)(_ + _.number)
  }

  override def part2(input: Grid[Char]): Int = {
    val stars = input.toList.collect { case (pos, char) if char == '*' => pos }
    val parts = getParts(input)
    stars.foldLeft(0) { (acc, star) =>
      val partsAdjacentToStars = parts.foldLeft(List.empty[Int]) {
        case (partsAcc, part) =>
          if (part.positions.toSet.intersect(star.neighbors.toSet).nonEmpty) {
            part.number :: partsAcc
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

  private def getParts(input: Grid[Char]): Vector[Part] = {
    def isSymbol(point: Point): Boolean = {
      val char = input.getOrElse(point, '.')
      char != '.' && !char.isDigit
    }

    val (parts, _, _) = input.toSeq.sortBy(_._1).foldLeft((Vector.empty[Part], 0, Vector.empty[Point])) {
      case ((partAcc, numAcc, posAcc), (pos, char)) =>
        if (char.isDigit) {
          (partAcc, 10 * numAcc + char.asDigit, posAcc :+ pos)
        } else {
          (partAcc :+ Part(numAcc, posAcc), 0, Vector.empty)
        }
    }

    parts.filter(_.positions.flatMap(_.neighbors).exists(isSymbol))
  }
}

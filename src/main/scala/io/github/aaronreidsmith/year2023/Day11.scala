package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day11 extends Solution {
  type I  = List[List[Char]]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[List[Char]] = file.getLines().toList.map(_.toList)
  override def part1(input: List[List[Char]]): Long       = solution(input, 2)
  override def part2(input: List[List[Char]]): Long       = solution(input, 1_000_000)

  private def solution(input: List[List[Char]], factor: Int): Long = {
    val emptyRows = input.zipWithIndex.collect { case (line, i) if line.forall(_ == '.') => i }
    val emptyCols = input.transpose.zipWithIndex.collect { case (col, i) if col.forall(_ == '.') => i }

    val galaxies = {
      for {
        (line, y) <- input.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char == '#'
      } yield Point(x, y)
    }

    galaxies.combinations(2).foldLeft(0L) {
      case (acc, List(a, b)) =>
        val Seq(left, right) = Seq(a.x, b.x).sorted
        val addedWidth = emptyCols.foldLeft(0) { (acc, col) =>
          if (col >= left && col <= right) {
            acc + factor - 1
          } else {
            acc
          }
        }

        val Seq(top, bottom) = Seq(a.y, b.y).sorted
        val addedHeight = emptyRows.foldLeft(0) { (acc, row) =>
          if (row >= top && row <= bottom) {
            acc + factor - 1
          } else {
            acc
          }
        }

        acc + right - left + addedWidth + bottom - top + addedHeight
      case (acc, _) => acc
    }
  }
}

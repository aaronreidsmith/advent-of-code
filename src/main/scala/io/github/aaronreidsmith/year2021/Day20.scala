package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day20 extends Solution {
  type I  = (Vector[Char], Grid[Char])
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): (Vector[Char], Grid[Char]) = {
    def translate(in: Char): Char = if (in == '.') '0' else '1'

    val Array(ieaRaw, gridRaw, _*) = file.mkString.trim.split("\n\n"): @unchecked
    val iea                        = ieaRaw.map(translate).toVector
    val grid                       = gridRaw.toGrid.view.mapValues(translate).toMap
    (iea, grid)
  }

  override def part1(input: (Vector[Char], Grid[Char])): Int = solution(input, 2)
  override def part2(input: (Vector[Char], Grid[Char])): Int = solution(input, 50)

  private def solution(input: (Vector[Char], Grid[Char]), times: Int): Int = {
    val (imageEnhancementAlgorithm, initialGrid) = input

    @tailrec
    def helper(grid: Grid[Char], default: Char = '0', iteration: Int = 0): Int = if (iteration >= times) {
      grid.values.count(_ == '1')
    } else {
      val (rows, cols) = grid.keys.unzip
      val minimumRow   = rows.min - 1
      val maximumRow   = rows.max + 1
      val minimumCol   = cols.min - 1
      val maximumCol   = cols.max + 1

      val updatedGrid = {
        for {
          col <- minimumCol to maximumCol
          row <- minimumRow to maximumRow
          position = Point(row, col)
        } yield {
          val lookupString = position.neighbors(includeSelf = true).map(grid.getOrElse(_, default)).mkString
          val lookupValue  = Integer.parseInt(lookupString, 2)
          val newValue     = imageEnhancementAlgorithm(lookupValue)
          position -> newValue
        }
      }.toMap
      val newDefault = if (default == '0') imageEnhancementAlgorithm.head else imageEnhancementAlgorithm.last

      helper(updatedGrid, newDefault, iteration + 1)
    }

    helper(initialGrid)
  }
}

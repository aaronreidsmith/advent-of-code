package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day20 {
  private implicit class Tuple2IntOps(position: (Int, Int)) {
    private val (row, col) = position

    def neighbors: Seq[(Int, Int)] = for {
      r <- Seq(row - 1, row, row + 1)
      c <- Seq(col - 1, col, col + 1)
    } yield (r, c)
  }

  def main(args: Array[String]): Unit = {
    val (imageEnhancementAlgorithm, initialGrid) = Using.resource(Source.fromResource("2021/day20.txt")) { file =>
      val Array(ieaRaw, gridRaw) = file.mkString.split("\n\n", 2)
      val iea = ieaRaw.map {
        case '.' => '0'
        case _   => '1'
      }.toVector
      val grid = {
        for {
          (line, row) <- gridRaw.split('\n').zipWithIndex
          (char, col) <- line.zipWithIndex
        } yield (row, col) -> (if (char == '.') '0' else '1')
      }.toMap

      (iea, grid)
    }

    def solution(initialGrid: Map[(Int, Int), Char], times: Int): Int = {
      @tailrec
      def helper(grid: Map[(Int, Int), Char], default: Char = '0', iteration: Int = 0): Int = if (iteration >= times) {
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
            position = (row, col)
          } yield {
            val lookupString = position.neighbors.map(grid.getOrElse(_, default)).mkString
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

    println(s"Part 1: ${solution(initialGrid, 2)}")
    println(s"Part 2: ${solution(initialGrid, 50)}")
  }
}

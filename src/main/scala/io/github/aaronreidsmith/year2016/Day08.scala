package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = String

  override def parseInput(file: Source): List[String] = file.getLines().toList
  override def part1(input: List[String]): Int        = solution(input)._1
  override def part2(input: List[String]): String     = solution(input)._2

  // Both solutions require the same traversal, so might as well only do it once
  private var solved = false
  private var answer = (0, "")
  private def solution(input: List[String]): (Int, String) = {
    if (!solved) {
      val rect      = "^rect (\\d+)x(\\d+)$".r
      val rotateCol = "^rotate column x=(\\d+) by (\\d+)$".r
      val rotateRow = "^rotate row y=(\\d+) by (\\d+)$".r

      def rotate(vector: Vector[Char], i: Int): Vector[Char] = {
        val size = vector.size
        vector.drop(size - (i % size)) ++: vector.take(size - (i % size))
      }

      @tailrec
      def helper(
          instructions: List[String],
          state: Vector[Vector[Char]] = Vector.fill(6)(Vector.fill(50)('.'))
      ): (Int, String) = instructions match {
        case Nil =>
          val display = new StringBuilder("\n")
          val numOn = state.foldLeft(0) { (acc, row) =>
            val newRow = s"${row.mkString.replace('.', ' ')}\n"
            display.appendAll(newRow)
            acc + row.count(_ == '#')
          }
          (numOn, display.mkString)
        case head :: tail =>
          val updatedState = head match {
            case rect(width, height) =>
              val coordinates = for {
                row <- 0 until height.toInt
                col <- 0 until width.toInt
              } yield (row, col)
              coordinates.foldLeft(state) {
                case (currentState, (row, col)) =>
                  val existingRow = currentState(row)
                  val updatedRow  = existingRow.updated(col, '#')
                  currentState.updated(row, updatedRow)
              }
            case rotateCol(column, amount) =>
              val colIndex     = column.toInt
              val rotatedState = state.transpose
              val row          = rotatedState(colIndex)
              rotatedState.updated(colIndex, rotate(row, amount.toInt)).transpose
            case rotateRow(row, amount) =>
              val rowIndex    = row.toInt
              val rowToUpdate = state(rowIndex)
              state.updated(rowIndex, rotate(rowToUpdate, amount.toInt))
            case _ => throw new IllegalArgumentException
          }
          helper(tail, updatedState)
      }

      answer = helper(input)
      solved = true
    }

    answer
  }
}

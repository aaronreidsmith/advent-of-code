package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source

object Day08 {
  private val rect      = "^rect (\\d+)x(\\d+)$".r("width", "height")
  private val rotateCol = "^rotate column x=(\\d+) by (\\d+)$".r("column", "amount")
  private val rotateRow = "^rotate row y=(\\d+) by (\\d+)$".r("row", "amount")

  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("2016/day08.txt")
    val instructions = input.getLines().toList
    input.close()

    val (part1, part2) = solution(instructions)
    println(s"Part 1: $part1")
    println(s"Part 2:\n$part2")
  }

  @tailrec
  def solution(
      instructions: List[String],
      state: Vector[Vector[Char]] = Vector.fill(6)(Vector.fill(50)('.'))
  ): (Int, String) = instructions match {
    case Nil =>
      state.foldLeft((0, "")) {
        case ((numOn, display), row) => (numOn + row.count(_ == '#'), display + s"${row.mkString.replace('.', ' ')}\n")
      }
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
      solution(tail, updatedState)
  }

  private def rotate(vector: Vector[Char], i: Int): Vector[Char] = {
    val size = vector.size
    vector.drop(size - (i % size)) ++: vector.take(size - (i % size))
  }
}

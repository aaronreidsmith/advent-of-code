package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day05 {
  def main(args: Array[String]): Unit = {
    val seats   = using("2020/day05.txt")(_.getLines().toList.map(findSeat))
    val maxSeat = seats.max
    println(s"Part 1: $maxSeat")

    val part2 = {
      val minSeat      = seats.min
      val allSeats     = (minSeat to maxSeat).toSet
      val missingSeats = allSeats.diff(seats.toSet)
      missingSeats.head
    }
    println(s"Part 2: $part2")
  }

  @tailrec
  private def binarySearch(boardingPass: String, possibleRows: List[Int], lowerSymbol: Char): Int = possibleRows match {
    case head :: Nil => head
    case _ =>
      val halfwayPoint = possibleRows.size / 2
      if (boardingPass.head == lowerSymbol) {
        binarySearch(boardingPass.tail, possibleRows.dropRight(halfwayPoint), lowerSymbol)
      } else {
        binarySearch(boardingPass.tail, possibleRows.drop(halfwayPoint), lowerSymbol)
      }
  }

  private def findSeat(boardingPass: String): Int = {
    val rowDefinition = boardingPass.take(7)
    val colDefinition = boardingPass.drop(7)
    val row           = binarySearch(rowDefinition, (0 until 128).toList, 'F')
    val col           = binarySearch(colDefinition, (0 until 8).toList, 'L')
    (row * 8) + col
  }
}

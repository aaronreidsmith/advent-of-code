package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[Int] = file.getLines().toList.map(findSeat)
  override def part1(input: List[Int]): Int        = input.max
  override def part2(input: List[Int]): Int        = (input.min to input.max).toSet.diff(input.toSet).head

  private def findSeat(boardingPass: String): Int = {
    @tailrec
    def binarySearch(boardingPass: String, possibleRows: List[Int], lowerSymbol: Char): Int = possibleRows match {
      case head :: Nil => head
      case _ =>
        val halfwayPoint = possibleRows.size / 2
        if (boardingPass.head == lowerSymbol) {
          binarySearch(boardingPass.tail, possibleRows.dropRight(halfwayPoint), lowerSymbol)
        } else {
          binarySearch(boardingPass.tail, possibleRows.drop(halfwayPoint), lowerSymbol)
        }
    }

    val rowDefinition = boardingPass.take(7)
    val colDefinition = boardingPass.drop(7)
    val row           = binarySearch(rowDefinition, (0 until 128).toList, 'F')
    val col           = binarySearch(colDefinition, (0 until 8).toList, 'L')
    (row * 8) + col
  }
}

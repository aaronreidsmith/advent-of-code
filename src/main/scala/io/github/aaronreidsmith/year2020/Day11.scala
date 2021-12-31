package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

// TODO: This is adapted from some Python code, so it is _not_ idiomatic whatsoever
object Day11 {
  def main(args: Array[String]): Unit = {
    val input = using("2020/day11.txt") { file =>
      file.getLines().toVector.map(_.toVector)
    }
    println(s"Part 1: ${solution(input, 4, onlyImmediate = true)}")
    println(s"Part 2: ${solution(input, 5, onlyImmediate = false)}")
  }

  private def solution(state: Vector[Vector[Char]], minOccupancy: Int, onlyImmediate: Boolean): Int = {
    val maxRow = state.length
    val maxCol = state.head.length
    val directions = Seq(
      (-1, 0), // N
      (-1, 1), // NE
      (0, 1),  // E
      (1, 1),  // SE
      (1, 0),  // S
      (1, -1), // SW
      (0, -1), // W
      (-1, -1) // NW
    )
    val validSeats = Set('#', 'L')

    def occupiedNeighbors(ferry: Vector[Vector[Char]], row: Int, col: Int): Int = {
      def neighbor(direction: (Int, Int)): Char = {
        var newRow = row
        var newCol = col
        while (true) {
          newRow += direction._1
          newCol += direction._2
          if (newRow < 0 || newRow >= maxRow || newCol < 0 || newCol >= maxCol) {
            return '.'
          } else {
            val seat = ferry(newRow)(newCol)
            if (validSeats.contains(seat) || onlyImmediate) {
              return seat
            }
          }
        }
        '.' // Default (never reached)
      }
      directions.map(neighbor).count(_ == '#')
    }

    @tailrec
    def helper(previousState: Vector[Vector[Char]]): Int = {
      val newState = (0 until maxRow).map { row =>
        (0 until maxCol).map { col =>
          val currentSeat  = previousState(row)(col)
          val occNeighbors = occupiedNeighbors(previousState, row, col)
          currentSeat match {
            case 'L' if occNeighbors == 0            => '#'
            case '#' if occNeighbors >= minOccupancy => 'L'
            case _                                   => currentSeat
          }
        }.toVector
      }.toVector
      if (newState == previousState) newState.flatten.count(_ == '#') else helper(newState)
    }

    helper(state)
  }
}

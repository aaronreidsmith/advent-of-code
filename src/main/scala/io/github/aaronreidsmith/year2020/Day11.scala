package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.implicits.SourceOps
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

// TODO: This is adapted from some Python code, so it is _not_ super idiomatic
object Day11 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Char] = file.toGrid
  override def part1(input: Grid[Char]): Int        = solution(input, minOccupancy = 4, onlyImmediate = true)
  override def part2(input: Grid[Char]): Int        = solution(input, minOccupancy = 5, onlyImmediate = false)

  private def solution(state: Grid[Char], minOccupancy: Int, onlyImmediate: Boolean): Int = {
    val directions = Seq(
      Point(-1, 0), // N
      Point(-1, 1), // NE
      Point(0, 1),  // E
      Point(1, 1),  // SE
      Point(1, 0),  // S
      Point(1, -1), // SW
      Point(0, -1), // W
      Point(-1, -1) // NW
    )

    def occupiedNeighbors(ferry: Grid[Char], pos: Point): Int = {
      def neighbor(direction: Point): Char = {
        var next        = pos
        var returnValue = '.'
        breakable {
          while (true) {
            next += direction
            ferry.get(next) match {
              case Some(seat) if seat == '#' || seat == 'L' || onlyImmediate =>
                returnValue = seat
                break()
              case None => break()
              case _    => // next iteration
            }
          }
        }
        returnValue
      }

      directions.map(neighbor).count(_ == '#')
    }

    @tailrec
    def helper(previousState: Grid[Char]): Int = {
      val newState = previousState.map {
        case (position, currentSeat) =>
          val occNeighbors = occupiedNeighbors(previousState, position)
          currentSeat match {
            case 'L' if occNeighbors == 0            => position -> '#'
            case '#' if occNeighbors >= minOccupancy => position -> 'L'
            case _                                   => position -> currentSeat
          }
      }
      if (newState == previousState) newState.values.count(_ == '#') else helper(newState)
    }

    helper(state)
  }
}

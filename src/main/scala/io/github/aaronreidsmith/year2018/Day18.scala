package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day18 extends Solution {
  type I  = Grid[Square]
  type O1 = Int
  type O2 = Int

  extension (grid: Grid[Square]) {
    def next: Grid[Square] = grid.foldLeft(grid) {
      case (acc, (position, Square.OpenGround)) =>
        val adjacent = position.neighbors.flatMap(grid.get)
        val next     = if (adjacent.count(_ == Square.Trees) >= 3) Square.Trees else Square.OpenGround
        acc.updated(position, next)
      case (acc, (position, Square.Trees)) =>
        val adjacent = position.neighbors.flatMap(grid.get)
        val next     = if (adjacent.count(_ == Square.Lumberyard) >= 3) Square.Lumberyard else Square.Trees
        acc.updated(position, next)
      case (acc, (position, Square.Lumberyard)) =>
        val adjacent = position.neighbors.flatMap(grid.get)
        val next = if (adjacent.count(_ == Square.Lumberyard) >= 1 && adjacent.count(_ == Square.Trees) >= 1) {
          Square.Lumberyard
        } else {
          Square.OpenGround
        }
        acc.updated(position, next)
    }
  }

  enum Square {
    case OpenGround, Trees, Lumberyard
  }

  override def parseInput(file: Source): Grid[Square] = file.toGrid.view.mapValues {
    case '|' => Square.Trees
    case '#' => Square.Lumberyard
    case _   => Square.OpenGround
  }.toMap

  override def part1(input: Grid[Square]): Int = simulate(input, 10)

  override def part2(input: Grid[Square]): Int = {
    val seenStates     = mutable.Set.empty[Grid[Square]]
    var startedUpState = Map.empty[Point, Square]
    var startUpCycles  = 0
    var loopSize       = 0

    @tailrec
    def helper(state: Grid[Square], iteration: Int = 0): Int = if (seenStates.contains(state)) {
      if (startUpCycles == 0) { // First, we have to find the number of cycles until we get to a consistent loop
        seenStates.clear()      // Clear seenStates so we can find another loop
        startUpCycles = iteration
        startedUpState = state
        helper(state)             // Run it with a new iteration counter
      } else if (loopSize == 0) { // Will only enter here once we have a consistent cycle and have found a second loop
        loopSize = iteration
        val limit = (1000000000 - startUpCycles) % loopSize
        simulate(startedUpState, limit) // Run that number of cycles from the beginning of the looping phase
      } else {
        -1 // Doesn't get here
      }
    } else {
      seenStates.add(state)
      helper(state.next, iteration + 1)
    }

    helper(input)
  }

  private def simulate(state: Grid[Square], iterations: Int): Int = {
    val finalState = Iterator.iterate(state)(_.next).take(iterations + 1).toSeq.last
    finalState.values.count(_ == Square.Trees) * finalState.values.count(_ == Square.Lumberyard)
  }
}

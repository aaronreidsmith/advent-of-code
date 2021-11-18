package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.util.FileUtils

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object Day18 extends FileUtils {
  private sealed trait Square
  private case object OpenGround extends Square
  private case object Trees      extends Square
  private case object Lumberyard extends Square

  def main(args: Array[String]): Unit = {
    val input = using(Source.fromResource("2018/day18.txt")) { file =>
      val lines = file.getLines().toList
      for {
        (line, row) <- lines.zipWithIndex
        (char, col) <- line.zipWithIndex
      } yield {
        val square = char match {
          case '|' => Trees
          case '#' => Lumberyard
          case _   => OpenGround
        }
        (row, col) -> square
      }
    }.toMap

    println(s"Part 1: ${part1(input, 10)}")

    val part2 = {
      val seenStates     = mutable.Set.empty[Map[(Int, Int), Square]]
      var startedUpState = Map.empty[(Int, Int), Square]
      var startUpCycles  = 0
      var loopSize       = 0

      @tailrec
      def helper(state: Map[(Int, Int), Square], iteration: Int = 0): Int = if (seenStates.contains(state)) {
        if (startUpCycles == 0) { // First, we have to find the number of cycles until we get to a consistent loop
          seenStates.clear()      // Clear seenStates so we can find another loop
          startUpCycles = iteration
          startedUpState = state
          helper(state)             // Run it with a new iteration counter
        } else if (loopSize == 0) { // Will only enter here once we have a consistent cycle and have found a second loop
          loopSize = iteration
          val limit = (1000000000 - startUpCycles) % loopSize
          part1(startedUpState, limit) // Run that number of cycles from the beginning of the looping phase
        } else {
          -1 // Doesn't get here
        }
      } else {
        seenStates.add(state)
        helper(nextState(state), iteration + 1)
      }

      helper(input)
    }
    println(s"Part 2: $part2")
  }

  @tailrec
  private def part1(state: Map[(Int, Int), Square], limit: Int, iteration: Int = 0): Int = if (iteration >= limit) {
    state.values.count(_ == Trees) * state.values.count(_ == Lumberyard)
  } else {
    part1(nextState(state), limit, iteration + 1)
  }

  private def nextState(state: Map[(Int, Int), Square]): Map[(Int, Int), Square] = {
    def neighbors(position: (Int, Int)): Seq[Square] = {
      val (row, col) = position
      for {
        currRow <- Seq(row - 1, row, row + 1) if currRow >= 0 && currRow < 50
        currCol <- Seq(col - 1, col, col + 1) if currCol >= 0 && currCol < 50
        if (currRow, currCol) != position
      } yield state((currRow, currCol))
    }

    state.foldLeft(state) {
      case (acc, (position, OpenGround)) =>
        val adjacent = neighbors(position)
        val next     = if (adjacent.count(_ == Trees) >= 3) Trees else OpenGround
        acc.updated(position, next)
      case (acc, (position, Trees)) =>
        val adjacent = neighbors(position)
        val next     = if (adjacent.count(_ == Lumberyard) >= 3) Lumberyard else Trees
        acc.updated(position, next)
      case (acc, (position, Lumberyard)) =>
        val adjacent = neighbors(position)
        val next =
          if (adjacent.count(_ == Lumberyard) >= 1 && adjacent.count(_ == Trees) >= 1) Lumberyard
          else OpenGround
        acc.updated(position, next)
    }
  }
}

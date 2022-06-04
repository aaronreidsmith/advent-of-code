package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using
import net.fornwall.aoc.Solver

import scala.annotation.tailrec
import scala.collection.mutable

object Day24 {
  def main(args: Array[String]): Unit = {
    val input = using("2019/day24.txt")(_.mkString)

    val part1 = {
      val seenStates = mutable.Set.empty[Map[(Int, Int), Boolean]]

      @tailrec
      def helper(state: Map[(Int, Int), Boolean]): Int = if (seenStates.contains(state)) {
        state.foldLeft(0) {
          case (acc, ((row, col), isBug)) if isBug =>
            val n = (row * 5) + col
            acc + math.pow(2, n).toInt
          case (acc, _) => acc
        }
      } else {
        seenStates.add(state)
        val nextState = {
          def numBugNeighbors(position: (Int, Int)): Int = {
            val (row, col) = position
            Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)).count(state.getOrElse(_, false))
          }

          state.foldLeft(state) {
            case (acc, (position, isBug)) if isBug && numBugNeighbors(position) != 1 => acc.updated(position, false)
            case (acc, (position, isBug)) if !isBug && Seq(1, 2).contains(numBugNeighbors(position)) =>
              acc.updated(position, true)
            case (acc, _) => acc
          }
        }
        helper(nextState)
      }

      val initialState = {
        for {
          (line, row) <- input.split('\n').zipWithIndex
          (char, col) <- line.zipWithIndex
        } yield (row, col) -> (char == '#')
      }.toMap

      helper(initialState)
    }
    println(s"Part 1: $part1")

    // TODO: Actually solve this
    println(s"Part 2: ${Solver.solve(2019, 24, 2, input)}")
  }
}

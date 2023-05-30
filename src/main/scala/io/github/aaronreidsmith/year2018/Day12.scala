package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends Solution {
  type I  = (Map[Int, Char], Map[String, Char])
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): (Map[Int, Char], Map[String, Char]) = {
    val Array(initialStateString, rulesString, _*) = file.mkString.trim.split("\n\n"): @unchecked
    val initialState = initialStateString.filter(char => char == '#' || char == '.').zipWithIndex.map(_.swap).toMap
    val rule         = "^(.*) => (.*)$".r
    val rules = rulesString.split('\n').foldLeft(Map.empty[String, Char]) {
      case (acc, rule(input, output)) => acc.updated(input, output.head)
      case (acc, _)                   => acc
    }

    (initialState, rules)
  }

  override def part1(input: (Map[Int, Char], Map[String, Char])): Int  = solution(input, 20)._1
  override def part2(input: (Map[Int, Char], Map[String, Char])): Long = solution(input, 1000)._2

  private def solution(input: (Map[Int, Char], Map[String, Char]), iterations: Int): (Int, Long) = {
    val (initialState, rules) = input

    def sumPottedPlants(pots: Map[Int, Char]): Int = pots.foldLeft(0) {
      case (acc, (pot, state)) if state == '#' => acc + pot
      case (acc, _)                            => acc
    }

    @tailrec
    def helper(
        pots: Map[Int, Char],
        iteration: Int = 0,
        previousSum: Int = 0,
        diffs: Vector[Int] = Vector()
    ): (Int, Long) = {
      if (iteration >= iterations) {
        val part1 = sumPottedPlants(pots)

        val last100Diff = diffs.sum / diffs.length
        val part2       = (50000000000L - iteration) * last100Diff + sumPottedPlants(pots)

        (part1, part2)
      } else {
        val minPot = pots.keys.min
        val maxPot = pots.keys.max
        val updatedPots = (minPot - 2 to maxPot + 2).foldLeft(Map.empty[Int, Char]) {
          case (acc, pot) =>
            val twoLeft       = pots.getOrElse(pot - 2, '.')
            val oneLeft       = pots.getOrElse(pot - 1, '.')
            val current       = pots.getOrElse(pot, '.')
            val oneRight      = pots.getOrElse(pot + 1, '.')
            val twoRight      = pots.getOrElse(pot + 2, '.')
            val extendedState = s"$twoLeft$oneLeft$current$oneRight$twoRight"
            val updatedState  = rules(extendedState)
            acc + (pot -> updatedState)
        }
        val currentSum   = sumPottedPlants(updatedPots)
        val diff         = currentSum - previousSum
        val updatedDiffs = (diffs :+ diff).takeRight(100)
        helper(updatedPots, iteration + 1, currentSum, updatedDiffs)
      }
    }

    helper(initialState)
  }
}

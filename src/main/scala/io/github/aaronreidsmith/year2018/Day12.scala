package io.github.aaronreidsmith.year2018

import scala.annotation.tailrec

object Day12 {
  def main(args: Array[String]): Unit = {
    // Just hardcoded from input
    val input        = "#...#..##.......####.#..###..#.##..########.#.#...#.#...###.#..###.###.#.#..#...#.#..##..#######.##"
    val initialState = input.zipWithIndex.map(_.swap).toMap
    println(s"Part 1: ${part1(initialState)}")
    println(s"Part 2: ${part2(initialState)}")
  }

  @tailrec
  private def part1(pots: Map[Int, Char], iteration: Int = 0): Int = if (iteration >= 20) {
    sumPottedPlants(pots)
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
        val updatedState  = nextState(extendedState)
        acc + (pot -> updatedState)
    }
    part1(updatedPots, iteration + 1)
  }

  @tailrec
  private def part2(pots: Map[Int, Char], iteration: Int = 0, previousSum: Int = 0, diffs: List[Int] = Nil): Long =
    if (iteration >= 1000) {
      val last100Diff = diffs.sum / diffs.length
      (50000000000L - iteration) * last100Diff + sumPottedPlants(pots)
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
          val updatedState  = nextState(extendedState)
          acc + (pot -> updatedState)
      }
      val currentSum   = sumPottedPlants(updatedPots)
      val diff         = currentSum - previousSum
      val updatedDiffs = (diffs :+ diff).takeRight(100)
      part2(updatedPots, iteration + 1, currentSum, updatedDiffs)
    }

  private def sumPottedPlants(pots: Map[Int, Char]): Int = pots.foldLeft(0) {
    case (acc, (pot, state)) => acc + (if (state == '#') pot else 0)
  }

  // Just hard-coded from input file
  private def nextState(state: String): Char = state match {
    case "#..#." => '#'
    case "#.#.." => '#'
    case "###.." => '#'
    case "##..#" => '.'
    case ".#.##" => '#'
    case "....." => '.'
    case "...#." => '#'
    case "##.#." => '#'
    case "#.#.#" => '.'
    case "###.#" => '#'
    case "....#" => '.'
    case "####." => '#'
    case ".##.." => '#'
    case "#.##." => '#'
    case "#..##" => '#'
    case "##..." => '#'
    case "#...#" => '.'
    case "##.##" => '#'
    case ".#..." => '.'
    case ".#..#" => '#'
    case "..#.#" => '#'
    case "#####" => '.'
    case ".####" => '#'
    case "..#.." => '#'
    case "#.###" => '.'
    case "..##." => '.'
    case ".##.#" => '#'
    case ".#.#." => '.'
    case "..###" => '.'
    case ".###." => '.'
    case "...##" => '.'
    case "#...." => '.'
    case other   => throw new IllegalArgumentException(other)
  }
}

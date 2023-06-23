package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day02 extends Solution {
  type I  = List[(Char, Char)]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[(Char, Char)] = {
    file
      .getLines()
      .map(line => (line.head, line.last))
      .toList
  }

  override def part1(rounds: List[(Char, Char)]): Int = rounds.foldLeft(0) {
    case (acc, (opponent, player)) =>
      val score = (opponent, player) match {
        case ('A', 'X') => 4 // Rock, Rock (3 + 1)
        case ('A', 'Y') => 8 // Rock, Paper (6 + 2)
        case ('A', 'Z') => 3 // Rock, Scissors (0 + 3)
        case ('B', 'X') => 1 // Paper, Rock (0 + 1)
        case ('B', 'Y') => 5 // Paper, Paper (3 + 2)
        case ('B', 'Z') => 9 // Paper, Scissors (6 + 3)
        case ('C', 'X') => 7 // Scissors, Rock (6 + 1)
        case ('C', 'Y') => 2 // Scissors, Paper (0 + 2)
        case ('C', 'Z') => 6 // Scissors, Scissors (3 + 3)
        case _          => throw new IllegalArgumentException
      }
      acc + score
  }

  override def part2(rounds: List[(Char, Char)]): Int = rounds.foldLeft(0) {
    case (acc, (opponent, outcome)) =>
      val score = (opponent, outcome) match {
        case ('A', 'X') => 3 // They chose Rock, I chose Scissors to Lose (3 + 0)
        case ('A', 'Y') => 4 // They chose Rock, I chose Rock to Draw (1 + 3)
        case ('A', 'Z') => 8 // They chose Rock, I chose Paper to Win (2 + 6)
        case ('B', 'X') => 1 // They chose Paper, I chose Rock to Lose (1 + 0)
        case ('B', 'Y') => 5 // They chose Paper, I chose Paper to Draw (2 + 3)
        case ('B', 'Z') => 9 // They chose Paper, I chose Scissors to Win (3 + 6)
        case ('C', 'X') => 2 // They chose Scissors, I chose Paper to Lose (2 + 0)
        case ('C', 'Y') => 6 // They chose Scissors, I chose Scissors to Draw (3 + 3)
        case ('C', 'Z') => 7 // They chose Scissors, I chose Rock to Win (1 + 6)
        case _          => throw new IllegalArgumentException
      }
      acc + score
  }
}

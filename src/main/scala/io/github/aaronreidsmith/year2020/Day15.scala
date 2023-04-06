package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.annotation.tailrec
import scala.io.Source

@Slow(part2 = true)
object Day15 extends Solution {
  type I  = Int => Int
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Int => Int = {
    val input = file.mkString.trim
      .split(',')
      .zipWithIndex
      .map { case (num, index) => num.toInt -> (index + 1) }
      .toMap

    playGame(input, input.size + 1, 0, _)
  }

  override def part1(input: Int => Int): Int = input(2020)
  override def part2(input: Int => Int): Int = input(30_000_000)

  @tailrec
  private def playGame(numbers: Map[Int, Int], turn: Int, number: Int, targetTurn: Int): Int = if (turn == targetTurn) {
    number
  } else {
    val nextNumber = numbers.get(number).map(turn - _).getOrElse(0)
    playGame(numbers.updated(number, turn), turn + 1, nextNumber, targetTurn)
  }
}

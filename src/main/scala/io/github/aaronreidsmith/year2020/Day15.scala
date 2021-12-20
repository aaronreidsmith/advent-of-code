package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day15 {
  def main(args: Array[String]): Unit = {
    val input = using("2020/day15.txt") { file =>
      file.mkString
        .split(',')
        .zipWithIndex
        .map { case (num, index) => num.toInt -> (index + 1) }
        .toMap
    }

    val solution = playGame(input, input.size + 1, 0, _)

    println(s"Part 1: ${solution(2020)}")
    println(s"Part 2: ${solution(30_000_000)}")
  }

  @tailrec
  private def playGame(numbers: Map[Int, Int], turn: Int, number: Int, targetTurn: Int): Int = if (turn == targetTurn) {
    number
  } else {
    val nextNumber = numbers.get(number).map(turn - _).getOrElse(0)
    playGame(numbers.updated(number, turn), turn + 1, nextNumber, targetTurn)
  }
}

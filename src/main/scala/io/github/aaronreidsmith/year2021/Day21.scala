package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21 extends Solution {
  type I  = (Int, Int)
  type O1 = Int
  type O2 = Long

  private implicit class PositionOps(position: Int) {
    def next(moves: Int): Int = {
      val newPos = (position + moves) % 10
      if (newPos == 0) 10 else newPos
    }
  }

  override def parseInput(file: Source): (Int, Int) = {
    val List(player1, player2, _*) = file.getLines().map(_.split(": ").last.toInt).toList
    (player1, player2)
  }

  override def part1(input: (Int, Int)): Int = {
    @tailrec
    def helper(pos1: Int, pos2: Int, score1: Int = 0, score2: Int = 0, rolls: Int = 0): Int = if (score2 >= 1000) {
      rolls * score1
    } else {
      val newPos1 = pos1.next(3 * rolls + 6)
      helper(pos2, newPos1, score2, score1 + newPos1, rolls + 3)
    }

    val (player1, player2) = input
    helper(player1, player2)
  }

  override def part2(input: (Int, Int)): Long = {
    val cache = mutable.Map.empty[(Int, Int, Int, Int), (Long, Long)]
    def helper(pos1: Int, pos2: Int, score1: Int = 0, score2: Int = 0): (Long, Long) = cache.getOrElseUpdate(
      (pos1, pos2, score1, score2),
      if (score2 >= 21) {
        (0, 1)
      } else {
        val moves = Seq((3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1))
        moves.foldLeft((0L, 0L)) {
          case ((wins1, wins2), (move, n)) =>
            val newPos1  = pos1.next(move)
            val (w2, w1) = helper(pos2, newPos1, score2, score1 + newPos1)
            (wins1 + n * w1, wins2 + n * w2)
          case (acc, _) => acc
        }
      }
    )

    val (player1, player2) = input
    val (p1Wins, p2Wins)   = helper(player1, player2)
    p1Wins.max(p2Wins)
  }
}

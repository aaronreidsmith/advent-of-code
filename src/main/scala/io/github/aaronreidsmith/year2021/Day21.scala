package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21 {
  def main(args: Array[String]): Unit = {
    val input   = Using.resource(Source.fromResource("2021/day21.txt"))(_.getLines().toList)
    val p1Start = input.head.filter(_.isDigit).toInt
    val p2Start = input.last.filter(_.isDigit).toInt

    println(s"Part 1: ${part1(p1Start, p2Start)}")
    println(s"Part 2: ${part2(p1Start, p2Start)}")
  }

  private def nextPosition(currentPosition: Int, moves: Int): Int = {
    val newPos = (currentPosition + moves) % 10
    if (newPos == 0) 10 else newPos
  }

  @tailrec
  private def part1(pos1: Int, pos2: Int, score1: Int = 0, score2: Int = 0, rolls: Int = 0): Int = if (score2 >= 1000) {
    rolls * score1
  } else {
    val newPos1 = nextPosition(pos1, 3 * rolls + 6)
    part1(pos2, newPos1, score2, score1 + newPos1, rolls + 3)
  }

  private def part2(initialPos1: Int, initialPos2: Int): Long = {
    val cache = mutable.Map.empty[(Int, Int, Int, Int), (Long, Long)]
    def helper(pos1: Int, pos2: Int, score1: Int = 0, score2: Int = 0): (Long, Long) = cache.getOrElseUpdate(
      (pos1, pos2, score1, score2),
      if (score2 >= 21) {
        (0, 1)
      } else {
        val moves = Seq((3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1))
        moves.foldLeft((0L, 0L)) {
          case ((wins1, wins2), (move, n)) =>
            val newPos1  = nextPosition(pos1, move)
            val (w2, w1) = helper(pos2, newPos1, score2, score1 + newPos1)
            (wins1 + n * w1, wins2 + n * w2)
        }
      }
    )

    val (p1Wins, p2Wins) = helper(initialPos1, initialPos2)
    p1Wins.max(p2Wins)
  }
}

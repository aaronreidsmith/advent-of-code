package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends Solution {
  type I  = IntCode
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): IntCode = IntCode(file)

  override def part1(input: IntCode): Int = {
    input.allOutput.zipWithIndex.count {
      case (value, index) if (index + 1) % 3 == 0 => value == 2
      case _ => false
    }
  }

  override def part2(input: IntCode): Long = {
    def take(intCode: IntCode, amount: Int): (IntCode, Seq[Long]) = {
      val raw    = Iterator.iterate(intCode)(_.nextOutput).slice(1, amount + 1).toSeq
      val output = raw.map(_.result).collect { case IntCode.Output(value) => value }
      (raw.last, output)
    }

    def step(
        score: Long,
        paddle: Long,
        ball: Long,
        tiles: Map[Point, Long] = Map.empty[Point, Long],
        output: Seq[Long]
    ): (Long, Long, Long, Map[Point, Long]) = {
      output.grouped(3).foldLeft((score, paddle, ball, tiles)) {
        case ((currentScore, currentPaddle, currentBall, currentTiles), Seq(x, y, id)) =>
          val nextScore  = if (x == -1) id else currentScore
          val nextPaddle = if (id == 3) x else currentPaddle
          val nextBall   = if (id == 4) x else currentBall
          (nextScore, nextPaddle, nextBall, currentTiles.updated(Point(x.toInt, y.toInt), id))
        case (acc, _) => acc
      }
    }

    @tailrec
    def play(
        computer: IntCode,
        amount: Int = 2595, // Trial and error to find this number
        score: Long = -1,
        paddle: Long = -1,
        ball: Long = -1,
        tiles: Map[Point, Long] = Map.empty[Point, Long]
    ): Long = {
      val (nextComputer, output)                       = take(computer, amount)
      val (nextScore, nextPaddle, nextBall, nextTiles) = step(score, paddle, ball, tiles, output)
      val finalComputer                                = nextComputer.withInput((nextBall - nextPaddle).sign)
      val nextBlocks                                   = nextTiles.values.count(_ == 2)
      if (nextBlocks == 0) nextScore else play(finalComputer, 6, nextScore, nextPaddle, nextBall, nextTiles)
    }

    play(input.copy(memory = input.memory.updated(0, 2)))
  }
}

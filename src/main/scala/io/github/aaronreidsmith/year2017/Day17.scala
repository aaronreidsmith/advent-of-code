package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends Solution(2017, 17) {
  type I  = Int
  type O1 = Int
  type O2 = Int

  override protected[year2017] def parseInput(file: Source): Int = file.mkString.trim.toInt

  override protected[year2017] def part1(input: Int): Int = {
    @tailrec
    def helper(
        stepCount: Int,
        currentPosition: Int = 0,
        currentNum: Int = 0,
        circularList: Vector[Int] = Vector(0)
    ): Int = if (currentNum == 2017) {
      circularList(currentPosition + 1)
    } else {
      val nextPosition = (currentPosition + stepCount) % circularList.size + 1
      val nextNum      = currentNum + 1
      val newList      = (circularList.take(nextPosition) :+ nextNum) ++: circularList.drop(nextPosition)
      helper(stepCount, nextPosition, nextNum, newList)
    }

    helper(input)
  }

  override protected[year2017] def part2(stepCount: Int): Int = (1 to 50_000_000)
    .foldLeft((0, 0)) {
      case ((current, second), i) =>
        val next = 1 + (current + stepCount) % i
        (next, if (next == 1) i else second)
      case (acc, _) => acc
    }
    ._2
}

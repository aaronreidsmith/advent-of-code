package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends Solution {
  type I = (Long, Long)
  type O1 = Long
  type O2 = Nothing

  override def parseInput(file: Source): (Long, Long) = {
    val Seq(cardKey, doorKey, _*) = file.getLines().map(_.toLong).toSeq: @unchecked
    (cardKey, doorKey)
  }

  override def part1(input: (Long, Long)): Long = {
    val (cardKey, doorKey) = input

    @tailrec
    def helper(handshake: Long, target: Long): Long = if (target == doorKey) {
      handshake
    } else {
      val newTarget = (target * 7) % 20201227
      val newHandshake = (handshake * cardKey) % 20201227
      helper(newHandshake, newTarget)
    }

    helper(1L, 1L)
  }
}

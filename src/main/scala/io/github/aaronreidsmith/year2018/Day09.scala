package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

// TODO: Adapted from my Python solution, so a decent amount of mutability
object Day09 extends Solution {
  type I = (Int, Int)
  type O1 = Long
  type O2 = Long

  extension (deque: mutable.ArrayDeque[Long]) {
    // https://docs.python.org/3/library/collections.html#collections.deque.rotate
    def rotate(n: Int): Unit = if (n > 0) {
      (n until 0 by -1).foreach { _ =>
        deque.prepend(deque.removeLast())
      }
    } else {
      (n until 0).foreach { _ =>
        deque.append(deque.removeHead())
      }
    }
  }

  override def parseInput(file: Source): (Int, Int) = {
    val regex                         = """^(\d+) players; last marble is worth (\d+) points$""".r
    val regex(maxPlayers, lastMarble) = file.mkString.trim: @unchecked
    (maxPlayers.toInt, lastMarble.toInt)
  }

  override def part1(input: (Int, Int)): Long = {
    val (maxPlayers, lastMarble) = input
    playGame(maxPlayers, lastMarble)
  }

  override def part2(input: (Int, Int)): Long = {
    val (maxPlayers, lastMarble) = input
    playGame(maxPlayers, lastMarble * 100)
  }

  private def playGame(maxPlayers: Int, lastMarble: Long): Long = {
    val scores = mutable.Map.empty[Long, Long].withDefaultValue(0)
    val circle = mutable.ArrayDeque(0L)

    (1L to lastMarble).foreach { marble =>
      if (marble % 23 == 0) {
        circle.rotate(7)
        val key           = marble % maxPlayers
        val existingScore = scores(key)
        val toAdd         = marble + circle.removeLast()
        scores.update(key, existingScore + toAdd)
        circle.rotate(-1)
      } else {
        circle.rotate(-1)
        circle.append(marble)
      }
    }

    scores.values.max
  }
}

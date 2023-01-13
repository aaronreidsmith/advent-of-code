package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import java.util
import scala.io.Source

// This a very Java way to do this, but it works 🤷
object Day19 extends Solution {
  type I  = Int
  type O1 = Int
  type O2 = Int

  private class Elf {
    var number: Int = _
    var gifts: Int  = 1
    var prev: Elf   = _
    var next: Elf   = _
  }
  private var start: Elf = _

  override def parseInput(file: Source): Int = file.mkString.trim.toInt

  override def part1(input: Int): Int = {
    (1 to input).foreach { number =>
      val newElf = new Elf
      newElf.number = number

      Option(start) match {
        case Some(_) =>
          val last = start.prev
          newElf.next = start
          start.prev = newElf
          newElf.prev = last
          last.next = newElf
        case None =>
          newElf.next = newElf
          newElf.prev = newElf
          start = newElf
      }
    }

    while (start.prev != start && start.next != start) {
      val elfToTheRight = start.next
      start.gifts += elfToTheRight.gifts
      start.next = elfToTheRight.next
      elfToTheRight.next.prev = start
      start = start.next
    }

    start.number
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/5j4lp1/comment/dbdf9mn
  override def part2(input: Int): Int = {
    val left  = new util.LinkedList[Int]
    val right = new util.LinkedList[Int]
    (1 to input).foreach { elf =>
      if (elf < (input / 2) + 1) {
        left.add(elf)
      } else {
        right.addFirst(elf)
      }
    }

    while (left.size() > 0 && right.size() > 0) {
      if (left.size() > right.size()) {
        left.removeLast()
      } else {
        right.removeLast()
      }

      right.addFirst(left.removeFirst())
      left.add(right.removeLast())
    }

    left.peek() match {
      case value if value != 0 => value
      case _                   => right.peek()
    }
  }
}

package io.github.aaronreidsmith.year2018

import scala.collection.mutable
import scala.util.control.Breaks._

// Adapted from https://www.reddit.com/r/adventofcode/comments/a86jgt/comment/ec8frrd
object Day21 {
  def main(args: Array[String]): Unit = {
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  private[year2018] def part1: Long = solution._1
  private[year2018] def part2: Long = solution._2

  // Not a general solution; a code representation of my Elf-code input
  private lazy val solution: (Long, Long) = {
    var part1 = -1L
    var part2 = -1L

    val seen = mutable.Set.empty[Long]
    val CS   = mutable.Set.empty[Long]

    val twoToThe16th = math.pow(2, 16).toLong
    val twoToThe24th = math.pow(2, 24).toLong
    val originalC    = 10736359L

    var C = originalC
    var D = 65536L
    breakable {
      while (true) {
        val E = D % 256
        C += E
        C = (C % twoToThe24th * 65899) % twoToThe24th
        if (D < 256) {
          if (CS.isEmpty) {
            part1 = C
          } else if (!CS.contains(C)) {
            part2 = C
          }
          CS.add(C)
          D = C | twoToThe16th
          if (seen.contains(D)) {
            break()
          }
          seen.add(D)
          C = originalC
        } else {
          D = D / 256
        }
      }
    }

    (part1, part2)
  }
}

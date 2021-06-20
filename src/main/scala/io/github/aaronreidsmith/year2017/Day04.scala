package io.github.aaronreidsmith.year2017

import scala.io.Source

object Day04 {
  def main(args: Array[String]): Unit = {
    val input       = Source.fromResource("2017/day04.txt")
    val passphrases = input.getLines().toList
    input.close()

    val part1 = passphrases.filter { line =>
      val words = line.split(' ')
      words.distinct.length == words.length
    }
    println(s"Part 1: ${part1.size}")

    val part2 = part1.count { line =>
      val words = line.split(' ').map(_.sorted)
      words.distinct.length == words.length
    }
    println(s"Part 2: $part2")
  }
}

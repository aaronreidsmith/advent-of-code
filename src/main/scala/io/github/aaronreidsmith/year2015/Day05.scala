package io.github.aaronreidsmith.year2015

import scala.io.Source

object Day05 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day05.txt")
    val words = input.getLines().toList
    input.close()

    val part1 = words.count(isNicePart1)
    println(s"Part 1: $part1")

    val part2 = words.count(isNicePart2)
    println(s"part 2: $part2")
  }

  private def isNicePart1(word: String): Boolean = {
    val badPairs = "ab|cd|pq|xy".r

    val threeVowels = word.replaceAll("[^aeiou]", "").length >= 3
    val doubles     = word.sliding(2).exists(pair => pair.head == pair.last)
    val noBadPairs  = badPairs.findFirstMatchIn(word).isEmpty

    threeVowels && doubles && noBadPairs
  }

  private def isNicePart2(word: String): Boolean = {
    val nonOverlappingSeq = word.sliding(2).exists(_.r.findAllMatchIn(word).length > 1)
    val headTailMatch     = word.sliding(3).exists(triple => triple.head == triple.last)
    nonOverlappingSeq && headTailMatch
  }
}

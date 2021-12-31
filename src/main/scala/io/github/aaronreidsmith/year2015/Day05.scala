package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

object Day05 {
  def main(args: Array[String]): Unit = {
    val words = using("2015/day05.txt")(_.getLines().toList)
    println(s"Part 1: ${part1(words)}")
    println(s"part 2: ${part2(words)}")
  }

  private[year2015] def part1(words: Seq[String]): Int = {
    val badPairs = "ab|cd|pq|xy".r

    words.count { word =>
      val threeVowels = word.replaceAll("[^aeiou]", "").length >= 3
      val doubles     = word.sliding(2).exists(pair => pair.head == pair.last)
      val noBadPairs  = badPairs.findFirstMatchIn(word).isEmpty

      threeVowels && doubles && noBadPairs
    }
  }

  private[year2015] def part2(words: Seq[String]): Int = words.count { word =>
    val nonOverlappingSeq = word.sliding(2).exists(_.r.findAllMatchIn(word).length > 1)
    val headTailMatch     = word.sliding(3).exists(triple => triple.head == triple.last)
    nonOverlappingSeq && headTailMatch
  }
}

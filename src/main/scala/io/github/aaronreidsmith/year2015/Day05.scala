package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day05 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2015, Day 5")
    val words = using("2015/day05.txt")(parseInput)
    println(s"Part 1: ${part1(words)}")
    println(s"part 2: ${part2(words)}")
    println()
  }

  override protected[year2015] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2015] def part1(words: List[String]): Int = {
    val badPairs = "ab|cd|pq|xy".r

    words.count { word =>
      val threeVowels = word.replaceAll("[^aeiou]", "").length >= 3
      val doubles     = word.sliding(2).exists(pair => pair.head == pair.last)
      val noBadPairs  = badPairs.findFirstMatchIn(word).isEmpty

      threeVowels && doubles && noBadPairs
    }
  }

  override protected[year2015] def part2(words: List[String]): Int = words.count { word =>
    val nonOverlappingSeq = word.sliding(2).exists(_.r.findAllMatchIn(word).length > 1)
    val headTailMatch     = word.sliding(3).exists(triple => triple.head == triple.last)
    nonOverlappingSeq && headTailMatch
  }
}

package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day01 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[String] = file.getLines().toList

  override def part1(input: List[String]): Int = input.foldLeft(0) { (acc, line) =>
    val digits = line.toVector.filter(_.isDigit)
    val first  = digits.head
    val last   = digits.last
    acc + s"$first$last".toInt
  }

  override def part2(input: List[String]): Int = {
    def toDigit(str: String): Int = {
      val valueMapping = Map(
        "one"   -> 1,
        "two"   -> 2,
        "three" -> 3,
        "four"  -> 4,
        "five"  -> 5,
        "six"   -> 6,
        "seven" -> 7,
        "eight" -> 8,
        "nine"  -> 9
      )
      if (str.forall(_.isDigit)) str.toInt else valueMapping.getOrElse(str, 0)
    }

    // Digits may overlap (e.g. 'sevenine'), so we need the lookahead
    val regex = "(?=([1-9]|one|two|three|four|five|six|seven|eight|nine))".r
    input.foldLeft(0) { (acc, line) =>
      val matches = regex.findAllIn(line).matchData.map(_.group(1)).toList
      val first   = matches.head
      val last    = matches.last
      acc + s"${toDigit(first)}${toDigit(last)}".toInt
    }
  }
}

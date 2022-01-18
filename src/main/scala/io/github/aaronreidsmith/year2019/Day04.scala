package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.implicits._
import io.github.aaronreidsmith.using

import scala.io.Source

object Day04 {
  private implicit class IntOps(int: Int) {
    private lazy val string = int.toString

    def alwaysIncreasing: Boolean = string.sliding(2).forall { pair =>
      val first  = pair.head.asDigit
      val second = pair.last.asDigit
      first <= second
    }

    def hasDoubleDigits: Boolean = string.sliding(2).exists { pair =>
      pair.head == pair.last
    }

    def hasExactDoubleDigits: Boolean = hasDoubleDigits && string.letterOccurrences.values.exists(_ == 2)
  }

  def main(args: Array[String]): Unit = {
    val inputRange = using("2019/day04.txt")(parseInput)
    println(s"Part 1: ${part1(inputRange)}")
    println(s"Part 2: ${part2(inputRange)}")
  }

  private[year2019] def parseInput(file: Source): Range = {
    val Array(min, max, _*) = file.mkString.split('-')
    min.toInt to max.toInt
  }

  private[year2019] def part1(range: Range): Int = range.count { num =>
    num.hasDoubleDigits && num.alwaysIncreasing
  }

  private[year2019] def part2(range: Range): Int = range.count { num =>
    num.hasExactDoubleDigits && num.alwaysIncreasing
  }
}

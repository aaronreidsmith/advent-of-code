package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.implicits._
import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day04 extends Solution(2019, 4) {
  type I  = Range
  type O1 = Int
  type O2 = Int

  private implicit class IntOps(int: Int) {
    private lazy val string = int.toString

    def alwaysIncreasing: Boolean = string.sliding(2).forall { pair =>
      val first  = pair.head.asDigit
      val second = pair.last.asDigit
      first <= second
    }

    def hasDoubleDigits: Boolean      = string.sliding(2).exists(pair => pair.head == pair.last)
    def hasExactDoubleDigits: Boolean = hasDoubleDigits && string.letterOccurrences.values.exists(_ == 2)
  }

  override protected[year2019] def parseInput(file: Source): Range = {
    val Array(min, max, _*) = file.mkString.trim.split('-')
    min.toInt to max.toInt
  }

  override protected[year2019] def part1(range: Range): Int = range.count { num =>
    num.hasDoubleDigits && num.alwaysIncreasing
  }

  override protected[year2019] def part2(range: Range): Int = range.count { num =>
    num.hasExactDoubleDigits && num.alwaysIncreasing
  }
}

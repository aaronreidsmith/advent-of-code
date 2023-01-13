package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends Solution {
  type I  = List[String]
  type O1 = String
  type O2 = Nothing

  override def parseInput(file: Source): List[String] = file.getLines().toList
  override def part1(input: List[String]): String = {
    val decimal = input.foldLeft(0L)(_ + snafuToDecimal(_))
    decimalToSnafu(decimal)
  }

  private def decimalToSnafu(n: Long): String = {
    if (n == 0) {
      ""
    } else {
      val suffix = n % 5 match {
        case 0     => "0"
        case 1     => "1"
        case 2     => "2"
        case 3     => "="
        case 4     => "-"
        case other => throw new IllegalArgumentException(other.toString)
      }
      decimalToSnafu((n + 2) / 5) + suffix
    }
  }

  private def snafuToDecimal(initial: String): Long = {
    @tailrec
    def helper(snafu: String, total: Long = 0L, multiplier: Long = 1L): Long = {
      if (snafu.isEmpty) {
        total
      } else {
        val digit = snafu.head match {
          case '2' => 2
          case '1' => 1
          case '0' => 0
          case '-' => -1
          case '=' => -2
          case _   => throw new IllegalArgumentException
        }
        helper(snafu.tail, total + digit * multiplier, multiplier * 5)
      }
    }

    helper(initial.reverse)
  }
}

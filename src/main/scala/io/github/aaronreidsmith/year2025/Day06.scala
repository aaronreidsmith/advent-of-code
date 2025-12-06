package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day06 extends Solution {
  type I  = List[String]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[String] = {
    file.getLines().toList
  }

  override def part1(input: List[String]): Long = {
    input.map(_.trim.split("\\s+").toVector).transpose.foldLeft(0L) { (acc, row) =>
      val nums = row.init.map(_.toLong)
      val fn   = char2Operator(row.last.head)
      acc + nums.reduceLeft(fn)
    }
  }

  override def part2(input: List[String]): Long = {
    @tailrec
    def helper(
        remaining: List[List[Char]],
        total: Long = 0L,
        currentSum: Long = 0L,
        currentFn: (Long, Long) => Long = _ + _
    ): Long = {
      remaining match {
        case Nil => total + currentSum
        // End of current problem, add to total and move to next one
        case row :: rest if row.forall(_.isSpaceChar) => helper(rest, total + currentSum)
        // Start of new problem, got a new function
        case row :: rest if row.last == '+' || row.last == '*' =>
          val num = row.filter(_.isDigit).mkString.toLong
          val fn  = char2Operator(row.last)
          helper(rest, total, num, fn)
        // In the middle of a problem
        case row :: rest =>
          val num = row.filter(_.isDigit).mkString.toLong
          helper(rest, total, currentFn(currentSum, num), currentFn)
      }
    }

    helper(input.reverse.transpose.map(_.reverse))
  }

  private def char2Operator(char: Char): (Long, Long) => Long = {
    char match {
      case '+'   => (a: Long, b: Long) => a + b
      case '*'   => (a: Long, b: Long) => a * b
      case other => throw new IllegalArgumentException(s"Unexpected operator: $other")
    }
  }
}

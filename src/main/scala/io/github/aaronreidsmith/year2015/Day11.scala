package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Solution {
  type I  = String
  type O1 = String
  type O2 = String

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): String     = solution(input)
  override def part2(input: String): String     = solution(incrementPassword(part1(input)))

  @tailrec
  private def solution(password: String): String =
    if (isValid(password)) password else solution(incrementPassword(password))

  private def isValid(password: String): Boolean = {
    val increasingRun = password.toCharArray.sliding(3).exists {
      case Array(a, b, c) => (a + 1).toChar == b && (b + 1).toChar == c
    }
    val noBadChars = password.forall(char => char != 'i' && char != 'o' && char != 'l')
    val twoOrMorePairs = password
      .sliding(2)
      .zipWithIndex
      .filter((pair, _) => pair.head == pair.last)
      .toList
      .combinations(2)
      .exists {
        case List((a, i), (b, j)) => a != b && j != i - 1 && j != i && j != i + 1
        case _                    => false
      }
    increasingRun && noBadChars && twoOrMorePairs
  }

  private def incrementPassword(current: String): String = current.lastOption match {
    case Some('z')  => incrementPassword(current.init) + 'a'
    case Some(char) => current.init + (char + 1).toChar
    case None       => ""
  }
}

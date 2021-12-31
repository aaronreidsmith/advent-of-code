package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    val input = "hxbxwxba"
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2015] def part1(input: String): String = solution(input)
  private[year2015] def part2(input: String): String = solution(incrementPassword(part1(input)))

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
      .filter { case (pair, _) => pair.head == pair.last }
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

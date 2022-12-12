package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks._

// Adapted from https://www.reddit.com/r/adventofcode/comments/a86jgt/comment/ec8frrd
object Day21 extends Solution(2018, 21) {
  type I  = List[String] // Unused
  type O1 = Long
  type O2 = Long

  override protected[year2018] def parseInput(file: Source): List[String] = file.getLines().toList
  override protected[year2018] def part1(input: List[String]): Long       = solution(input)._1
  override protected[year2018] def part2(input: List[String]): Long       = solution(input)._2

  private var answer = (0L, 0L)
  private var solved = false
  private def solution(input: List[String]): (Long, Long) = {
    if (!solved) {
      var part1 = -1L
      var part2 = -1L

      val seen = mutable.Set.empty[Long]
      val CS   = mutable.Set.empty[Long]

      val twoToThe16th = math.pow(2, 16).toLong
      val twoToThe24th = math.pow(2, 24).toLong
      val originalC    = 10736359L

      var C = originalC
      var D = 65536L
      breakable {
        while (true) {
          val E = D % 256
          C += E
          C = (C % twoToThe24th * 65899) % twoToThe24th
          if (D < 256) {
            if (CS.isEmpty) {
              part1 = C
            } else if (!CS.contains(C)) {
              part2 = C
            }
            CS.add(C)
            D = C | twoToThe16th
            if (seen.contains(D)) {
              break()
            }
            seen.add(D)
            C = originalC
          } else {
            D = D / 256
          }
        }
      }

      answer = (part1, part2)
      solved = true
    }

    answer
  }
}

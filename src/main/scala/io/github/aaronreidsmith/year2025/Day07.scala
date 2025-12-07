package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day07 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): List[String] = {
    file.getLines().toList
  }

  override def part1(input: List[String]): Int  = solution(input)._1
  override def part2(input: List[String]): Long = solution(input)._2

  private var solved      = false
  private var part1Answer = 0
  private var part2Answer = 0L
  private def solution(input: List[String]): (Int, Long) = {
    if (solved) {
      (part1Answer, part2Answer)
    } else {
      val timelines = mutable.Seq.from(input.head.map {
        case 'S' => 1L
        case _   => 0L
      })
      for {
        line      <- input.tail
        (char, i) <- line.zipWithIndex
        if char == '^'
      } {
        if (timelines(i) > 0) {
          part1Answer += 1
        }
        timelines(i - 1) += timelines(i)
        timelines(i + 1) += timelines(i)
        timelines(i) = 0L
      }
      part2Answer = timelines.sum
      solved = true
      (part1Answer, part2Answer)
    }
  }
}

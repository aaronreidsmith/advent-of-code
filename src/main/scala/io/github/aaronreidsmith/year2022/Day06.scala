package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day06 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): String = file.mkString
  override def part1(input: String): Int        = solution(input, 4)
  override def part2(input: String): Int        = solution(input, 14)

  private def solution(input: String, markerSize: Int): Int = input.zipWithIndex
    .sliding(markerSize)
    .collectFirst {
      case group if group.map(_._1).distinct.size == markerSize => group.map(_._2).last + 1
    }
    .get
}

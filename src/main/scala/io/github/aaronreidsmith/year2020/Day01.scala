package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day01 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[Int] = file.getLines().map(_.toInt).toList
  override def part1(input: List[Int]): Int        = solution(input, 2)
  override def part2(input: List[Int]): Int        = solution(input, 3)

  private def solution(input: List[Int], groupSize: Int): Int = {
    input.combinations(groupSize).collectFirst { case group if group.sum == 2020 => group.product }.get
  }
}

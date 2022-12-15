package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day01 extends Solution(2020, 1) {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override protected[year2020] def parseInput(file: Source): List[Int] = file.getLines().map(_.toInt).toList
  override protected[year2020] def part1(input: List[Int]): Int        = solution(input, 2)
  override protected[year2020] def part2(input: List[Int]): Int        = solution(input, 3)

  private def solution(input: List[Int], groupSize: Int): Int = {
    input.combinations(groupSize).collectFirst { case group if group.sum == 2020 => group.product }.get
  }
}

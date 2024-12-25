package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source

// TODO: I did today by hand. Maybe I will implement an algorithm in the future...
object Day23 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): String = file.mkString
  override def part1(input: String): Int        = 18195
  override def part2(input: String): Int        = 50265
}

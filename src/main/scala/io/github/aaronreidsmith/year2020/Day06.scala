package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day06 extends Solution(2020, 6) {
  type I = List[String]
  type O1 = Int
  type O2 = Int

  override protected[year2020] def parseInput(file: Source): List[String] = {
    file.mkString.trim.split("\n\n").toList
  }

  override protected[year2020] def part1(input: List[String]): Int = {
    input.foldLeft(0)(_ + _.replaceAll("\n", "").toSet.size)
  }

  override protected[year2020] def part2(input: List[String]): Int = {
    input.foldLeft(0)((acc, entry) => acc + entry.split('\n').map(_.toSet).reduceLeft(_.intersect(_)).size)
  }
}

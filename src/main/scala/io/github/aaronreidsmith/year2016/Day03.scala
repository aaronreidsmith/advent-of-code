package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day03 extends Solution {
  type I  = List[List[Int]] // We use an inner list instead of a tuple for easy transposition
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[List[Int]] = {
    file
      .getLines()
      .toList
      .map(_.trim.split("\\s+").take(3).toList.map(_.toInt))
  }

  override def part1(input: List[List[Int]]): Int = input.count(isTriangle)

  override def part2(input: List[List[Int]]): Int = input.transpose.foldLeft(0) { (acc, column) =>
    acc + column.grouped(3).count(isTriangle)
  }

  private def isTriangle(triple: List[Int]): Boolean = {
    val a :: b :: c :: _ = triple: @unchecked
    (a + b) > c && (a + c) > b && (b + c) > a
  }
}

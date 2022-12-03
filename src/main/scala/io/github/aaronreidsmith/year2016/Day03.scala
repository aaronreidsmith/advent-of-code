package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day03 extends Solution {
  type I  = List[List[Int]] // We use an inner list instead of a tuple for easy transposition
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2016, Day 3")
    val input = using("2016/day03.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): List[List[Int]] = {
    file
      .getLines()
      .toList
      .map(_.trim.split("\\s+").take(3).toList.map(_.toInt))
  }

  override protected[year2016] def part1(input: List[List[Int]]): Int = input.count(isTriangle)

  override protected[year2016] def part2(input: List[List[Int]]): Int = input.transpose.foldLeft(0) { (acc, column) =>
    acc + column.grouped(3).count(isTriangle)
  }

  private def isTriangle(triple: List[Int]): Boolean = {
    val a :: b :: c :: _ = triple
    (a + b) > c && (a + c) > b && (b + c) > a
  }
}

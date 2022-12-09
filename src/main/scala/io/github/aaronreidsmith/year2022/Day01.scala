package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day01 extends Solution {
  type I = Seq[Seq[Int]]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2022, Day 1")
    val input = using("2022/day01.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2022] def parseInput(file: Source): Seq[Seq[Int]] = {
    file.mkString
      .split("\n\n")
      .toSeq
      .map { block =>
        block
          .split("\n")
          .toSeq
          .map(_.toInt)
      }
  }
  override protected[year2022] def part1(elves: Seq[Seq[Int]]): Int = elves.map(_.sum).max
  override protected[year2022] def part2(elves: Seq[Seq[Int]]): Int = elves.map(_.sum).sorted.takeRight(3).sum
}

package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day01.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2022] def parseInput(file: Source): Seq[Seq[Int]] = {
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
  private[year2022] def part1(elves: Seq[Seq[Int]]): Int = elves.map(_.sum).max
  private[year2022] def part2(elves: Seq[Seq[Int]]): Int = elves.map(_.sum).sorted.takeRight(3).sum
}

package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day01 extends Solution {
  type I = Seq[Seq[Int]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Seq[Seq[Int]] = {
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
  override def part1(elves: Seq[Seq[Int]]): Int = elves.map(_.sum).max
  override def part2(elves: Seq[Seq[Int]]): Int = elves.map(_.sum).sorted.takeRight(3).sum
}

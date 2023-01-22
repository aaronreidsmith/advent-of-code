package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.util.Using

object Day01 extends Solution {
  type I  = Seq[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Seq[Int] = file.getLines().map(_.toInt).toSeq

  override def part1(input: Seq[Int]): Int = input.sliding(2).count {
    case Seq(a, b) => b > a
    case _         => false
  }

  // (A + B + C) - (B + C + D) = A - D, so we only need to compare A and D
  override def part2(input: Seq[Int]): Int = input.sliding(4).count {
    case Seq(a, _, _, d) => d > a
    case _               => false
  }
}

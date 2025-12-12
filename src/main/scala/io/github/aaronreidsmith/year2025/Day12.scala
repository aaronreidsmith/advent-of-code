package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day12 extends Solution {
  type I  = Seq[(Int, Seq[Int])]
  type O1 = Int
  type O2 = Nothing

  override def parseInput(file: Source): Seq[(Int, Seq[Int])] = {
    val region = """^(\d+)x(\d+): ([\d\s]+)$""".r
    file
      .getLines()
      .collect {
        case region(width, height, presents) =>
          ((width.toInt / 3) * (height.toInt / 3), presents.split(' ').toSeq.map(_.toInt))
      }
      .toSeq
  }

  override def part1(input: Seq[(Int, Seq[Int])]): Int = {
    input.count((capacity, presents) => capacity >= presents.sum)
  }
}

package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day20 extends Solution {
  type I  = Vector[(Long, Long)]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): Vector[(Long, Long)] = {
    val (starts, ends) = file
      .getLines()
      .toVector
      .map { line =>
        val Array(start, end, _*) = line.split('-')
        (start.toLong, end.toLong)
      }
      .unzip

    (starts.sorted :+ 4294967296L).zip(0L +: ends.sorted)
  }

  override def part1(input: Vector[(Long, Long)]): Long = {
    input
      .find { case (start, end) => start > end + 1 }
      .map { case (_, end) => end + 1 }
      .getOrElse(-1)
  }

  override def part2(input: Vector[(Long, Long)]): Long = {
    input.foldLeft(0L) {
      case (acc, (start, end)) => acc + math.max(0, start - end - 1)
      case (acc, _)            => acc
    }
  }
}

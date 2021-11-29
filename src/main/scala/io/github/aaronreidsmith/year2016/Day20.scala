package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day20 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2016/day20.txt")
    val (starts, ends) = input
      .getLines()
      .map { line =>
        val Array(start, end, _*) = line.split('-')
        (start.toLong, end.toLong)
      }
      .toList
      .unzip
    input.close()

    val sortedIntervals = (starts.sorted :+ 4294967296L).zip(0L :: ends.sorted)

    val part1 = sortedIntervals
      .find { case (start, end) => start > end + 1 }
      .map { case (_, end) => end + 1 }
      .getOrElse(-1)
    println(s"Part 1: $part1")

    val part2 = sortedIntervals.foldLeft(0L) { case (acc, (start, end)) => acc + math.max(0, start - end - 1) }
    println(s"Part 2: $part2")
  }
}

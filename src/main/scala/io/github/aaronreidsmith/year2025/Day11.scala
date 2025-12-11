package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day11 extends Solution {
  type I  = Map[String, Seq[String]]
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): Map[String, Seq[String]] = {
    val paths = """^([a-z]{3}): ([a-z\s]+)$""".r
    file
      .getLines()
      .collect { case paths(entry, exits) => entry -> exits.split(' ').toSeq }
      .toMap
      .updated("out", Seq.empty)
  }

  override def part1(input: Map[String, Seq[String]]): Int = {
    solution(input)("you", "out")
  }

  override def part2(input: Map[String, Seq[String]]): Long = {
    val paths = solution(input)
    (
      paths("svr", "dac").toLong *
        paths("dac", "fft").toLong *
        paths("fft", "out").toLong
    ) + (
      paths("svr", "fft").toLong *
        paths("fft", "dac").toLong *
        paths("dac", "out").toLong
    )
  }

  private def solution(graph: Map[String, Seq[String]])(start: String, end: String): Int = {
    val cache = mutable.Map.empty[String, Int]
    def helper(here: String): Int = cache.getOrElseUpdate(
      here,
      if (here == end) {
        1
      } else {
        graph(here).foldLeft(0)((acc, next) => acc + helper(next))
      }
    )

    helper(start)
  }
}

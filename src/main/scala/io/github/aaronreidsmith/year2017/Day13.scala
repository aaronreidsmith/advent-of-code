package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.language.implicitConversions

object Day13 extends Solution {
  type I  = List[Scanner]
  type O1 = Int
  type O2 = Int

  case class Scanner(depth: Int, range: Int) {
    def detected(delay: Int): Boolean = (depth + delay) % (2 * range - 2) == 0
    val severity: Int                 = if (detected(0)) depth * range else 0
  }

  override def parseInput(file: Source): List[Scanner] = {
    val scanner = """^(\d+): (\d+)$""".r
    file.getLines().toList.collect { case scanner(depth, range) => Scanner(depth.toInt, range.toInt) }
  }

  override def part1(input: List[Scanner]): Int = input.foldLeft(0)(_ + _.severity)
  override def part2(input: List[Scanner]): Int = {
    LazyList.from(0).find(delay => input.forall(!_.detected(delay))).get
  }
}

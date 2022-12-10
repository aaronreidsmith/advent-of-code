package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source
import scala.language.implicitConversions

object Day13 extends Solution {
  type I  = List[Scanner]
  type O1 = Int
  type O2 = Int

  private[year2017] case class Scanner(depth: Int, range: Int) {
    def detected(delay: Int): Boolean = (depth + delay) % (2 * range - 2) == 0
    val severity: Int                 = if (detected(0)) depth * range else 0
  }

  def run(): Unit = {
    println("Year 2017, Day 13")
    val input = using("2017/day13.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2017] def parseInput(file: Source): List[Scanner] = {
    val scanner = """^(\d+): (\d+)$""".r
    file.getLines().toList.collect { case scanner(depth, range) => Scanner(depth.toInt, range.toInt) }
  }

  override protected[year2017] def part1(input: List[Scanner]): Int = input.foldLeft(0)(_ + _.severity)
  override protected[year2017] def part2(input: List[Scanner]): Int = {
    LazyList.from(0).find(delay => input.forall(!_.detected(delay))).get
  }
}

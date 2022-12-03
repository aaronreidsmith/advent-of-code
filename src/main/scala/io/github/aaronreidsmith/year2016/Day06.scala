package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day06 extends Solution {
  type I  = List[String]
  type O1 = String
  type O2 = String

  def run(): Unit = {
    println("Year 2016, Day 6")
    val input = using("2016/day06.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2016] def part1(input: List[String]): String = input.transpose.map { line =>
    line.groupBy(identity).maxBy { case (_, occurrences) => occurrences.length }._1
  }.mkString

  override protected[year2016] def part2(input: List[String]): String = input.transpose.map { line =>
    line.groupBy(identity).minBy { case (_, occurrences) => occurrences.length }._1
  }.mkString
}

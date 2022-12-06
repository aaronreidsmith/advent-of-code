package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day04 extends Solution {
  type I  = List[List[String]]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2017, Day 4")
    val input = using("2017/day04.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2017] def parseInput(file: Source): List[List[String]] = {
    file.getLines().toList.map(_.split(' ').toList)
  }

  override protected[year2017] def part1(input: List[List[String]]): Int = {
    input.count(words => words.distinct.length == words.length)
  }

  override protected[year2017] def part2(input: List[List[String]]): Int = input.count { words =>
    val sorted = words.map(_.sorted)
    sorted.distinct.length == sorted.length
  }
}

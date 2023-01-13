package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day04 extends Solution {
  type I  = List[List[String]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[List[String]] = {
    file.getLines().toList.map(_.split(' ').toList)
  }

  override def part1(input: List[List[String]]): Int = {
    input.count(words => words.distinct.length == words.length)
  }

  override def part2(input: List[List[String]]): Int = input.count { words =>
    val sorted = words.map(_.sorted)
    sorted.distinct.length == sorted.length
  }
}

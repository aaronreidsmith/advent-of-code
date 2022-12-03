package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day03.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): List[String] = file.getLines().toList

  protected[year2022] def part1(input: List[String]): Int = input.foldLeft(0) { (acc, rucksack) =>
    val length        = rucksack.length
    val (left, right) = rucksack.splitAt(length / 2)
    val overlap       = left.intersect(right).head
    acc + priority(overlap)
  }

  protected[year2022] def part2(input: List[String]): Int = input.grouped(3).foldLeft(0) { (acc, triplet) =>
    val a :: b :: c :: _ = triplet
    val badge            = a.intersect(b).intersect(c).head
    acc + priority(badge)
  }

  private def priority(char: Char): Int = char.toInt - (if (char.isLower) 96 else 38)
}

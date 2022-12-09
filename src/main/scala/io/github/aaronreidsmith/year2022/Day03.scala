package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day03 extends Solution {
  type I = List[String]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2022, Day 3")
    val input = using("2022/day03.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2022] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2022] def part1(input: List[String]): Int = input.foldLeft(0) { (acc, rucksack) =>
    val length        = rucksack.length
    val (left, right) = rucksack.splitAt(length / 2)
    val overlap       = left.intersect(right).head
    acc + priority(overlap)
  }

  override protected[year2022] def part2(input: List[String]): Int = input.grouped(3).foldLeft(0) { (acc, triplet) =>
    val a :: b :: c :: _ = triplet
    val badge            = a.intersect(b).intersect(c).head
    acc + priority(badge)
  }

  private def priority(char: Char): Int = char.toInt - (if (char.isLower) 96 else 38)
}

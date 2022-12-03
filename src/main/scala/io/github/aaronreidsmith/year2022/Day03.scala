package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.io.Source

object Day03 {
  private implicit class CharOps(char: Char) {
    def priority: Int = char.toInt - (if (char.isLower) 96 else 38)
  }

  def main(args: Array[String]): Unit = {
    val input = using("2022/day03.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): List[List[Int]] = {
    file.getLines().toList.map(line => line.map(_.priority).toList)
  }

  protected[year2022] def part1(input: List[List[Int]]): Int = input.foldLeft(0) { (acc, rucksack) =>
    val length  = rucksack.length
    val left    = rucksack.take(length / 2).toSet
    val right   = rucksack.drop(length / 2).toSet
    val overlap = left.intersect(right).head
    acc + overlap
  }

  protected[year2022] def part2(input: List[List[Int]]): Int = input.grouped(3).foldLeft(0) { (acc, triplet) =>
    val a :: b :: c :: _ = triplet.map(_.toSet)
    val badge            = a.intersect(b).intersect(c).head
    acc + badge
  }
}

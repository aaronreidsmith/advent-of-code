package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.io.Source

object Day06 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day06.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): String = file.mkString
  protected[year2022] def part1(input: String): Int        = solution(input, 4)
  protected[year2022] def part2(input: String): Int        = solution(input, 14)

  private def solution(input: String, markerSize: Int): Int = input.zipWithIndex
    .sliding(markerSize)
    .collectFirst {
      case quartet if quartet.map(_._1).distinct.size == markerSize => quartet.map(_._2).last + 1
    }
    .get
}

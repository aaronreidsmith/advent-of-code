package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.io.Source

object Day04 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day04.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): List[(Range, Range)] = {
    file
      .getLines()
      .toList
      .map { line =>
        val Array(left, right, _*)        = line.split(',')
        val Array(leftMin, leftMax, _*)   = left.split('-').map(_.toInt)
        val Array(rightMin, rightMax, _*) = right.split('-').map(_.toInt)
        (leftMin to leftMax, rightMin to rightMax)
      }
  }

  protected[year2022] def part1(input: List[(Range, Range)]): Int = input.count {
    case (left, right) =>
      val intersection = left.intersect(right)
      left == intersection || right == intersection
    case _ => false
  }

  protected[year2022] def part2(input: List[(Range, Range)]): Int = input.count {
    case (left, right) => left.intersect(right).nonEmpty
    case _             => false
  }
}

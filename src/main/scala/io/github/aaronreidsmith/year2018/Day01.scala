package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = using("2018/day01.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2018] def parseInput(file: Source): Vector[Int] = file.getLines().toVector.map(_.toInt)
  private[year2018] def part1(numbers: Vector[Int]): Int      = numbers.sum
  private[year2018] def part2(initialNumbers: Vector[Int]): Int = {
    @tailrec
    def helper(numbers: Vector[Int], pointer: Int = 0, currentFrequency: Int = 0, seen: Set[Int] = Set()): Int = {
      if (seen.contains(currentFrequency)) {
        currentFrequency
      } else {
        val nextFrequency = currentFrequency + numbers(pointer)
        val nextPointer   = if (pointer == numbers.size - 1) 0 else pointer + 1
        helper(numbers, nextPointer, nextFrequency, seen + currentFrequency)
      }
    }

    helper(initialNumbers)
  }
}

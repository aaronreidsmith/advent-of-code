package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends Solution(2018, 1) {
  type I  = Vector[Int]
  type O1 = Int
  type O2 = Int

  override protected[year2018] def parseInput(file: Source): Vector[Int] = file.getLines().toVector.map(_.toInt)
  override protected[year2018] def part1(input: Vector[Int]): Int        = input.sum
  override protected[year2018] def part2(input: Vector[Int]): Int = {
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

    helper(input)
  }
}

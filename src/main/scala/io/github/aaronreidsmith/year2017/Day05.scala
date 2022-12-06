package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends Solution {
  type I  = Vector[Int]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2017, Day 5")
    val input = using("2017/day05.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2017] def parseInput(file: Source): Vector[Int] = file.getLines().toVector.map(_.toInt)
  override protected[year2017] def part1(input: Vector[Int]): Int        = solution(input, part2 = false)
  override protected[year2017] def part2(input: Vector[Int]): Int        = solution(input, part2 = true)

  @tailrec
  private def solution(instructions: Vector[Int], part2: Boolean, index: Int = 0, steps: Int = 0): Int = {
    if (index < 0 || index >= instructions.size) {
      steps
    } else {
      val instruction = instructions(index)
      val nextIndex   = instruction + index
      // format: off
      val updatedInstructions = instructions.take(index) ++: (if (part2 && instruction >= 3) instruction - 1 else instruction + 1) +: instructions.drop(index + 1)
      // format: on
      solution(updatedInstructions, part2, nextIndex, steps + 1)
    }
  }
}

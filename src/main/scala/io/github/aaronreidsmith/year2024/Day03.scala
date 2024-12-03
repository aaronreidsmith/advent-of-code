package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day03 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  private val mul      = """mul\((\d{1,3}),(\d{1,3})\)""".r
  private val combined = """mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)""".r

  override def parseInput(file: Source): String = file.mkString

  override def part1(input: String): Int = {
    mul.findAllIn(input).foldLeft(0) {
      case (acc, mul(left, right)) => acc + (left.toInt * right.toInt)
      case (acc, _)                => acc
    }
  }

  override def part2(input: String): Int = {
    combined
      .findAllIn(input)
      .foldLeft((0, true)) {
        case ((acc, _), "do()")                            => (acc, true)
        case ((acc, _), "don't()")                         => (acc, false)
        case ((acc, enabled), mul(left, right)) if enabled => (acc + (left.toInt * right.toInt), enabled)
        case (acc, _)                                      => acc
      }
      ._1
  }
}

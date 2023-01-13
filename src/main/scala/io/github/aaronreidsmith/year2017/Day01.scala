package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day01 extends Solution {
  type I  = Vector[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Vector[Int] = file.mkString.trim.map(_.asDigit).toVector

  override def part1(input: Vector[Int]): Int = input.appended(input.head).sliding(2).foldLeft(0) {
    case (acc, Vector(a, b)) if a == b => acc + a
    case (acc, _)                      => acc
  }

  override def part2(input: Vector[Int]): Int = {
    val stepSize = input.length / 2
    val rotated  = input.drop(stepSize) ++ input.take(stepSize)
    input.zip(rotated).foldLeft(0) {
      case (acc, (a, b)) if a == b => acc + a
      case (acc, _)                => acc
    }
  }
}

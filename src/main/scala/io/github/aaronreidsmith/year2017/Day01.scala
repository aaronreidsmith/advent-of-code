package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day01 extends Solution {
  type I  = Vector[Int]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2017, Day 1")
    val input = using("2017/day01.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2017] def parseInput(file: Source): Vector[Int] = file.mkString.map(_.asDigit).toVector

  override protected[year2017] def part1(input: Vector[Int]): Int = input.appended(input.head).sliding(2).foldLeft(0) {
    case (acc, Vector(a, b)) if a == b => acc + a
    case (acc, _)                      => acc
  }

  override protected[year2017] def part2(input: Vector[Int]): Int = {
    val stepSize = input.length / 2
    val rotated  = input.drop(stepSize) ++ input.take(stepSize)
    input.zip(rotated).foldLeft(0) {
      case (acc, (a, b)) if a == b => acc + a
      case (acc, _)                => acc
    }
  }
}

package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day04 extends Solution(2022, 4) {
  type I = List[(Set[Int], Set[Int])]
  type O1 = Int
  type O2 = Int

  override protected[year2022] def parseInput(file: Source): List[(Set[Int], Set[Int])] = {
    file
      .getLines()
      .toList
      .map { line =>
        val Array(left, right, _*)        = line.split(',')
        val Array(leftMin, leftMax, _*)   = left.split('-').map(_.toInt)
        val Array(rightMin, rightMax, _*) = right.split('-').map(_.toInt)
        ((leftMin to leftMax).toSet, (rightMin to rightMax).toSet)
      }
  }

  override protected[year2022] def part1(input: List[(Set[Int], Set[Int])]): Int = input.count {
    case (left, right) => left.subsetOf(right) || right.subsetOf(left)
    case _             => false
  }

  override protected[year2022] def part2(input: List[(Set[Int], Set[Int])]): Int = input.count {
    case (left, right) => left.intersect(right).nonEmpty
    case _             => false
  }
}

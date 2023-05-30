package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day04 extends Solution {
  type I  = List[(Set[Int], Set[Int])]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[(Set[Int], Set[Int])] = {
    file
      .getLines()
      .toList
      .map { line =>
        val Array(left, right, _*)        = line.split(','): @unchecked
        val Array(leftMin, leftMax, _*)   = left.split('-').map(_.toInt): @unchecked
        val Array(rightMin, rightMax, _*) = right.split('-').map(_.toInt): @unchecked
        ((leftMin to leftMax).toSet, (rightMin to rightMax).toSet)
      }
  }

  override def part1(input: List[(Set[Int], Set[Int])]): Int = input.count { (left, right) =>
    left.subsetOf(right) || right.subsetOf(left)
  }

  override def part2(input: List[(Set[Int], Set[Int])]): Int = input.count { (left, right) =>
    left.intersect(right).nonEmpty
  }
}

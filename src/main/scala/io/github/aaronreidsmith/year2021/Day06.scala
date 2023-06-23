package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day06 extends Solution {
  type I  = Int => Long
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): Int => Long = {
    val input        = file.mkString.trim.split(',').map(_.toLong)
    val emptyFishMap = (0L to 8L).map(_ -> 0L).toMap
    val fishMap      = input.foldLeft(emptyFishMap)((acc, n) => acc.updated(n, acc(n) + 1))

    def nextGeneration(fish: Map[Long, Long]): Map[Long, Long] = {
      val rotated      = fish.map((k, v) => k - 1 -> v)
      val updatedItems = Map(-1L -> 0L, 6L -> (rotated(6) + rotated(-1)), 8L -> rotated(-1))
      rotated ++ updatedItems
    }

    (n: Int) => (0 until n).foldLeft(fishMap)((acc, _) => nextGeneration(acc)).values.sum
  }

  override def part1(input: Int => Long): Long = input(80)
  override def part2(input: Int => Long): Long = input(256)
}

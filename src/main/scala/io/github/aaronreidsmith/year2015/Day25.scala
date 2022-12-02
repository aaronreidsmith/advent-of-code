package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

object Day25 extends Solution {
  type I  = (Int, Int)
  type O1 = Long
  type O2 = Unit

  def run(): Unit = {
    println("Year 2015, Day 25")
    val input = (2978, 3083)
    println(s"Part 1: ${part1(input)}")
  }

  override protected[year2015] def part1(input: (Int, Int)): Long = {
    val (row, col) = input
    val index = {
      val n        = row + col - 1
      val topRight = n * (n + 1) / 2
      topRight - row
    }
    LazyList.iterate(20151125L)(ticket => (ticket * 252533) % 33554393).drop(index).head
  }
}

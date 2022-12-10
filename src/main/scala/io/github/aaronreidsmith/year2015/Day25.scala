package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day25 extends Solution(2015, 25) {
  type I  = (Int, Int)
  type O1 = Long
  type O2 = Unit

  override protected[year2015] def parseInput(file: Source): (Int, Int) = {
    val Array(row, col, _*) = file.mkString.split(',')
    (row.toInt, col.toInt)
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

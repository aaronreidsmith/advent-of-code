package io.github.aaronreidsmith.year2015

object Day25 {
  def main(args: Array[String]): Unit = {
    println(s"Part 1: ${part1(2978, 3083)}")
  }

  private[year2015] def part1(row: Int, col: Int): Long = {
    val index = {
      val n        = row + col - 1
      val topRight = n * (n + 1) / 2
      topRight - row
    }
    LazyList.iterate(20151125L)(ticket => (ticket * 252533) % 33554393).drop(index).head
  }
}

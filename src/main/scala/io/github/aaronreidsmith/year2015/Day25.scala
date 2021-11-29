package io.github.aaronreidsmith.year2015

object Day25 {
  def main(args: Array[String]): Unit = {
    val inputRow = 2978
    val inputCol = 3083

    val index = {
      val n        = inputRow + inputCol - 1
      val topRight = n * (n + 1) / 2
      topRight - inputRow
    }
    val ticket = LazyList.iterate(20151125L)(ticket => (ticket * 252533) % 33554393).drop(index).head

    println(s"Part 1: $ticket")
  }
}

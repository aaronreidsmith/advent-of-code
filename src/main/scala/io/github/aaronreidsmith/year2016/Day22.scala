package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day22 {
  protected[this] case class Node(used: Int, available: Int) {
    private val totalSpace = used + available

    def isEmpty: Boolean  = used == 0
    def nonEmpty: Boolean = !isEmpty

    override def toString: String = {
      val _used = if (used == 0) "__" else used
      // High capacity nodes are basically walls since they are already full
      if (totalSpace >= 500) "|||||" else s"${_used}/$totalSpace"
    }
  }

  private val nodeEntry = "^/dev/grid/node-x(\\d+)-y(\\d+)\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%$".r

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2016/day22.txt")
    val nodes = input.getLines().drop(2).foldLeft(Map.empty[(Int, Int), Node]) { (acc, line) =>
      line match {
        case nodeEntry(x, y, used, available) =>
          val (row, col) = (x.toInt, y.toInt)
          acc + ((row, col) -> Node(used.toInt, available.toInt))
        case other => throw new IllegalArgumentException(other)
      }
    }
    input.close()

    val part1 = {
      for {
        node  <- nodes.values
        other <- nodes.values if other != node && node.nonEmpty && node.used <= other.available
      } yield 1
    }.sum
    println(s"Part 1: $part1")

    // Solved by hand like so: https://www.reddit.com/r/adventofcode/comments/5jor9q/comment/dbhvxkp/
    // 62 steps to move hole to bottom left, then 29 cycles of 5 to move it to the top left. 29 * 5 + 62 = 207
    printGrid(nodes)
  }

  private def printGrid(grid: Map[(Int, Int), Node]): Unit = {
    var row = 0
    grid.toSeq
      .sortBy { case (position, _) => position }
      .foreach {
        case ((currRow, _), node) =>
          if (currRow != row) {
            row += 1
            println()
          }
          print(s"$node ")
      }
  }
}

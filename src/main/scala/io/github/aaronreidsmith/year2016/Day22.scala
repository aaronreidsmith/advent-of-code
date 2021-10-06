package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day22 {
  protected[this] case class Node(row: Int, col: Int, used: Int, available: Int) {
    lazy val neighbors: Seq[(Int, Int)] = Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter {
      case (r, c) => 0 <= r && r <= 30 && 0 <= c && c <= 30 // 30 is hard-coded from the input
    }

    def isEmpty: Boolean  = used == 0
    def nonEmpty: Boolean = !isEmpty
  }

  private val nodeEntry = "^/dev/grid/node-x(\\d+)-y(\\d+)\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%$".r

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2016/day22.txt")
    val nodes = input.getLines().drop(2).foldLeft(Map.empty[(Int, Int), Node]) { (acc, line) =>
      line match {
        case nodeEntry(x, y, used, available) =>
          val (row, col) = (x.toInt, y.toInt)
          acc + ((row, col) -> Node(row, col, used.toInt, available.toInt))
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
  }
}

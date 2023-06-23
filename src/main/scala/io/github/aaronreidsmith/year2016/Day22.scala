package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.nowarn
import scala.io.Source

object Day22 extends Solution {
  type I  = Map[Point, Node]
  type O1 = Int
  type O2 = Int

  case class Node(used: Int, available: Int) {
    private val totalSpace = used + available

    def isEmpty: Boolean  = used == 0
    def nonEmpty: Boolean = used != 0

    override def toString: String = {
      val _used = if (isEmpty) "__" else used
      // High capacity nodes are basically walls since they are already full
      if (totalSpace >= 500) "|||||" else s"${_used}/$totalSpace"
    }
  }

  override def parseInput(file: Source): Map[Point, Node] = {
    val nodeEntry = "^/dev/grid/node-x(\\d+)-y(\\d+)\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%$".r

    file
      .getLines()
      .drop(2)
      .foldLeft(Map.empty[Point, Node]) {
        case (acc, nodeEntry(x, y, used, available)) =>
          val (row, col) = (x.toInt, y.toInt)
          acc + (Point(row, col) -> Node(used.toInt, available.toInt))
        case (acc, _) => acc
      }
  }

  override def part1(input: Map[Point, Node]): Int = input.values.foldLeft(0) {
    case (acc, node) if node.nonEmpty =>
      acc + input.values.count(other => other != node && node.used <= other.available)
    case (acc, _) => acc
  }

  override def part2(input: Map[Point, Node]): Int = {
    @nowarn
    def printGrid(grid: Map[Point, Node]): Unit = {
      var row = 0
      grid.toSeq
        .sortBy((position, _) => (position.x, position.y))
        .foreach { (pos, node) =>
          if (pos.x != row) {
            row += 1
            println()
          }
          print(s"$node ")
        }
    }

    // TODO: Solved by hand like so: https://www.reddit.com/r/adventofcode/comments/5jor9q/comment/dbhvxkp/.
    //  Maybe I will do this via code some day
    // printGrid(input)
    207
  }
}

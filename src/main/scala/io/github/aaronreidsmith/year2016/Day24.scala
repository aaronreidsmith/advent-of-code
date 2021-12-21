package io.github.aaronreidsmith.year2016

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

// The majority of this is adapted from https://www.reddit.com/r/adventofcode/comments/5k1he1/comment/dbknd6b
object Day24 {
  private implicit class Tuple2Ops(tuple: (Int, Int)) {
    private val (row, col) = tuple

    private def left: (Int, Int)  = (row, col - 1)
    private def right: (Int, Int) = (row, col + 1)
    private def up: (Int, Int)    = (row - 1, col)
    private def down: (Int, Int)  = (row + 1, col)

    def neighbors: Seq[(Int, Int)] = Seq(left, right, up, down)
  }

  def main(args: Array[String]): Unit = {
    val grid = Using
      .resource(Source.fromResource("2016/day24.txt")) { file =>
        {
          for {
            (line, row) <- file.getLines().zipWithIndex
            (char, col) <- line.zipWithIndex
          } yield (row, col) -> char
        }.toMap
      }
      .withDefaultValue('#')
    val startPosition = grid.collectFirst { case (position, value) if value == '0' => position }.get
    // Find all non-zero positions
    val targetPositions =
      ('1' to '7').map(char => grid.collectFirst { case (position, value) if value == char => position }.get)
    // Precompute distance from 0 to all other numbers
    val distanceFromZero = targetPositions.map { target => bfsFromTo(grid, startPosition, target) }
    // Precompute distance from 0 to all other numbers
    val K = distanceFromZero.length
    val distances = {
      for {
        i <- 0 until K
        j <- i + 1 until K
        distance = bfsFromTo(grid, targetPositions(i), targetPositions(j))
      } yield Seq((i, j) -> distance, (j, i) -> distance)
    }.flatten.toMap

    // Iterate through all possible paths and find the shortest one for each part
    val (part1, part2) = (0 until 7).permutations.foldLeft((Int.MaxValue, Int.MaxValue)) {
      case ((part1Acc, part2Acc), path) =>
        val distance = path.sliding(2).foldLeft(distanceFromZero(path.head)) {
          case (acc, Seq(a, b)) => acc + distances((a, b))
        }
        val newPart1 = Seq(part1Acc, distance).min
        val newPart2 = Seq(part2Acc, distance + distanceFromZero(path.last)).min
        (newPart1, newPart2)
    }
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  // Hate this function, but not smart enough to refactor it, lol
  private def bfsFromTo(grid: Map[(Int, Int), Char], from: (Int, Int), to: (Int, Int)): Int = {
    val q       = mutable.ArrayDeque((0, from))
    val visited = mutable.Set(from)
    while (q.nonEmpty) {
      val (destination, current) = q.removeLast()
      if (current == to) {
        return destination
      }
      current.neighbors.foreach { neighbor =>
        if (grid(neighbor) != '#' && !visited.contains(neighbor)) {
          q.prepend((destination + 1, neighbor))
          visited.add(neighbor)
        }
      }
    }
    -1
  }
}

package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11 {
  implicit class Tuple2IntOps(tuple: (Int, Int)) {
    def neighbors: Set[(Int, Int)] = {
      val (row, col) = tuple
      for {
        r <- Seq(row - 1, row, row + 1)
        c <- Seq(col - 1, col, col + 1)
        if (r, c) != tuple
      } yield (r, c)
    }.toSet
  }

  def main(args: Array[String]): Unit = {
    val grid = Using
      .resource(Source.fromResource("2021/day11.txt")) { file =>
        for {
          (line, row) <- file.getLines().toList.zipWithIndex
          (char, col) <- line.zipWithIndex
        } yield (row, col) -> char.asDigit
      }
      .toMap
    val states = LazyList.iterate(grid)(nextState)

    val part1 = states.take(101).foldLeft(0)((acc, state) => acc + state.values.count(_ == 0))
    println(s"Part 1: $part1")

    val part2 = states.indexWhere(_.values.toSet.size == 1)
    println(s"Part 2: $part2")
  }

  private def nextState(grid: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val firstPass = grid.view.mapValues(_ + 1).toMap
    val flashes   = findFlashes(firstPass, firstPass.filter { case (_, value) => value > 9 }.keySet)
    firstPass.map {
      case (position, value) =>
        position -> (if (flashes.contains(position)) 0 else value + position.neighbors.count(flashes.contains))
    }
  }

  @tailrec
  private def findFlashes(grid: Map[(Int, Int), Int], flashes: Set[(Int, Int)]): Set[(Int, Int)] = {
    val next = flashes ++ flashes.flatMap(_.neighbors).filter { candidate =>
      grid.get(candidate).exists(_ + candidate.neighbors.intersect(flashes).size > 9)
    }
    if (flashes == next) flashes else findFlashes(grid, next)
  }
}

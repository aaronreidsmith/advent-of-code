package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.implicits.SourceOps
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Solution {
  type I  = LazyList[Grid[Int]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): LazyList[Grid[Int]] = {
    @tailrec
    def findFlashes(grid: Grid[Int], flashes: Set[Point]): Set[Point] = {
      val next = flashes ++ flashes.flatMap(_.neighbors).filter { candidate =>
        grid.get(candidate).exists(_ + candidate.neighbors.toSet.intersect(flashes).size > 9)
      }
      if (flashes == next) flashes else findFlashes(grid, next)
    }

    def nextState(grid: Grid[Int]): Grid[Int] = {
      val firstPass = grid.view.mapValues(_ + 1).toMap
      val flashes   = findFlashes(firstPass, firstPass.filter { case (_, value) => value > 9 }.keySet)
      firstPass.map {
        case (position, value) =>
          position -> (if (flashes.contains(position)) 0 else value + position.neighbors.count(flashes.contains))
      }
    }

    val grid = file.toGrid.view.mapValues(_.asDigit).toMap
    LazyList.iterate(grid)(nextState)
  }

  // format: off
  override def part1(input: LazyList[Grid[Int]]): Int = input.take(101).foldLeft(0)((acc, state) => acc + state.values.count(_ == 0))
  override def part2(input: LazyList[Grid[Int]]): Int = input.indexWhere(_.values.toSet.size == 1)
}

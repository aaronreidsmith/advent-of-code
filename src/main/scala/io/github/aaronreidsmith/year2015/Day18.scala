package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day18 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day18.txt")
    val grid = input.getLines().zipWithIndex.foldLeft(Map.empty[(Int, Int), Char]) {
      case (acc, (line, row)) =>
        acc ++ line.zipWithIndex.foldLeft(Map.empty[(Int, Int), Char]) {
          case (rowAcc, (char, col)) => rowAcc + ((row, col) -> char)
        }
    }
    input.close()
    println(s"Part 1: ${part1(grid)}")

    // Make sure all four corners have their lights on to start
    val updated = grid ++ Map((0, 0) -> '#', (0, 99) -> '#', (99, 0) -> '#', (99, 99) -> '#')
    println(s"Part 2: ${part2(updated)}")
  }

  @tailrec
  private def part1(grid: Map[(Int, Int), Char], iteration: Int = 0): Int = if (iteration >= 100) {
    grid.values.count(_ == '#')
  } else {
    val updated = grid.foldLeft(Map.empty[(Int, Int), Char]) {
      case (acc, ((row, col), char)) =>
        val isOn        = char == '#'
        val neighborsOn = getNeighbors(row, col).count { case (x, y) => grid(x, y) == '#' }
        val newState = if (isOn) {
          if (neighborsOn != 2 && neighborsOn != 3) '.' else '#'
        } else {
          if (neighborsOn == 3) '#' else '.'
        }
        acc + ((row, col) -> newState)
    }
    part1(updated, iteration + 1)
  }

  @tailrec
  private def part2(grid: Map[(Int, Int), Char], iteration: Int = 0): Int = if (iteration >= 100) {
    grid.values.count(_ == '#')
  } else {
    val alwaysOn = Seq((0, 0), (0, 99), (99, 0), (99, 99))
    val updated = grid.foldLeft(Map.empty[(Int, Int), Char]) {
      case (acc, ((row, col), char)) =>
        if (alwaysOn.contains((row, col))) {
          acc + ((row, col) -> '#')
        } else {
          val isOn        = char == '#'
          val neighborsOn = getNeighbors(row, col).count { case (x, y) => grid(x, y) == '#' }
          val newState = if (isOn) {
            if (neighborsOn != 2 && neighborsOn != 3) '.' else '#'
          } else {
            if (neighborsOn == 3) '#' else '.'
          }
          acc + ((row, col) -> newState)
        }
    }
    part2(updated, iteration + 1)
  }

  private def getNeighbors(row: Int, col: Int): Seq[(Int, Int)] = for {
    x <- Seq(row - 1, row, row + 1) if 0 <= x && x < 100
    y <- Seq(col - 1, col, col + 1) if 0 <= y && y < 100
    if (x, y) != (row, col)
  } yield (x, y)
}

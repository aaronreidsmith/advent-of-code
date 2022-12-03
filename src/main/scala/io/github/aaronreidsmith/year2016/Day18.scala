package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day18 extends Solution {
  type I  = Vector[Vector[Tile]]
  type O1 = Int
  type O2 = Int

  protected[year2016] case class Tile(isSafe: Boolean) {
    val isTrap: Boolean = !isSafe
  }

  private object Tile {
    def apply(char: Char): Tile = Tile(char == '.')
  }

  def run(): Unit = {
    println("Year 2016, Day 18")
    val input = using("2016/day18.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): Vector[Vector[Tile]] = {
    file
      .getLines()
      .toVector
      .map(line => line.toVector.map(Tile(_)))
  }

  override protected[year2016] def part1(input: Vector[Vector[Tile]]): Int = solution(input, 40)
  override protected[year2016] def part2(input: Vector[Vector[Tile]]): Int = solution(input, 400000)

  @tailrec
  private def solution(tiles: Vector[Vector[Tile]], threshold: Int): Int = if (tiles.length >= threshold) {
    tiles.foldLeft(0)((acc, row) => acc + row.count(_.isSafe))
  } else {
    val lastRow = Tile(true) +: tiles.last :+ Tile(true) // add our "safe" tiles on each end
    val newRow = lastRow.sliding(3).foldLeft(Vector.empty[Tile]) {
      case (acc, Vector(left, center, right)) =>
        val newTile =
          if (
            (left.isTrap && center.isTrap && right.isSafe) ||
            (left.isSafe && center.isTrap && right.isTrap) ||
            (left.isTrap && center.isSafe && right.isSafe) ||
            (left.isSafe && center.isSafe && right.isTrap)
          ) {
            Tile(false)
          } else {
            Tile(true)
          }
        acc :+ newTile
      case (acc, _) => acc
    }
    solution(tiles :+ newRow, threshold)
  }
}

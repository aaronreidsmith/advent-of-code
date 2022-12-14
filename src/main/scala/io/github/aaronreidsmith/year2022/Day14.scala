package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Grid, Point, using}

import scala.annotation.tailrec
import scala.io.Source

object Day14 {
  private[year2022] sealed trait Tile
  private case object Air  extends Tile
  private case object Rock extends Tile
  private case object Sand extends Tile

  def main(args: Array[String]): Unit = {
    val input = using("2022/day14.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  // The input has y increase from left to right and x increase downward,
  // but we swap them to play nice with our `Point` class
  private[year2022] def parseInput(file: Source): Grid[Tile] = {
    val point = """^(\d+),(\d+)$""".r
    file
      .getLines()
      .foldLeft(Map.empty[Point, Tile]) { (acc, line) =>
        val segments = line.split(" -> ").sliding(2).flatMap {
          case Array(point(aX, aY), point(bX, bY)) =>
            val minX = aX.toInt.min(bX.toInt)
            val maxX = aX.toInt.max(bX.toInt)
            val minY = aY.toInt.min(bY.toInt)
            val maxY = aY.toInt.max(bY.toInt)
            for {
              x <- minX to maxX
              y <- minY to maxY
            } yield Point(x, -y) -> Rock
          case _ => Nil
        }
        acc ++ segments
      }
      .withDefaultValue(Air)
  }

  private[year2022] def part1(input: Grid[Tile]): Int = {
    val maxDepth = input.keys.map(_.y).min
    settle(input, maxDepth).values.count(_ == Sand)
  }

  private[year2022] def part2(input: Grid[Tile]): Int = {
    val maxDepth = input.keys.map(_.y).min - 2
    // 0 to 1000 is arbitrary. Really only needs to be |maxDepth| in either direction from 500,
    // but with depth being negative, it looks kind of ugly
    val floor = (0 to 1000).map(Point(_, maxDepth) -> Rock)
    settle(input ++ floor, maxDepth).values.count(_ == Sand)
  }

  private def settle(grid: Grid[Tile], maxDepth: Int, sandPosition: Point = Point(500, 0)): Grid[Tile] = {
    @tailrec
    def helper(tiles: Grid[Tile], moves: List[Point]): Grid[Tile] = moves match {
      case Nil => tiles.updated(sandPosition, Sand)
      case head :: tail =>
        val newTiles = settle(tiles, maxDepth, head)
        newTiles(head) match {
          case Air         => newTiles
          case Sand | Rock => helper(newTiles, tail)
        }
    }

    if (sandPosition.y < maxDepth) {
      grid
    } else {
      grid(sandPosition) match {
        case Sand | Rock => grid
        case Air         => helper(grid, List(sandPosition.down, sandPosition.down.left, sandPosition.down.right))
      }
    }
  }
}

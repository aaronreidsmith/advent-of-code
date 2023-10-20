package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends Solution {
  type I  = Grid[Tile]
  type O1 = Int
  type O2 = Int

  enum Tile {
    case Air, Rock, Sand
  }

  // We switch x and y to play nice with our Point clTile.ass
  override def parseInput(file: Source): Grid[Tile] = {
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
            } yield Point(y, x) -> Tile.Rock
          case _ => Nil
        }
        acc ++ segments
      }
      .withDefaultValue(Tile.Air)
  }

  override def part1(input: Grid[Tile]): Int = {
    val maxDepth = input.keys.map(_.x).max
    settle(input, maxDepth).values.count(_ == Tile.Sand)
  }

  override def part2(input: Grid[Tile]): Int = {
    val maxDepth = input.keys.map(_.x).max + 2
    // 0 to 1000 is arbitrary. Really only needs to be |maxDepth| in either direction from 500
    val floor = (0 to 1000).map(Point(maxDepth, _) -> Tile.Rock)
    settle(input ++ floor, maxDepth).values.count(_ == Tile.Sand)
  }

  private def settle(grid: Grid[Tile], maxDepth: Int, sandPosition: Point = Point(0, 500)): Grid[Tile] = {
    @tailrec
    def helper(tiles: Grid[Tile], moves: List[Point]): Grid[Tile] = moves match {
      case Nil => tiles.updated(sandPosition, Tile.Sand)
      case head :: tail =>
        val newTiles = settle(tiles, maxDepth, head)
        newTiles(head) match {
          case Tile.Air              => newTiles
          case Tile.Sand | Tile.Rock => helper(newTiles, tail)
        }
    }

    if (sandPosition.x > maxDepth) {
      grid
    } else {
      grid(sandPosition) match {
        case Tile.Sand | Tile.Rock => grid
        case Tile.Air => helper(grid, List(sandPosition.down, sandPosition.down.left, sandPosition.down.right))
      }
    }
  }
}

package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends Solution(2022, 14) {
  type I  = Grid[Tile]
  type O1 = Int
  type O2 = Int

  private[year2022] sealed trait Tile
  private case object Air  extends Tile
  private case object Rock extends Tile
  private case object Sand extends Tile

  // We switch x and y to play nice with our Point class
  override protected[year2022] def parseInput(file: Source): Grid[Tile] = {
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
            } yield Point(y, x) -> Rock
          case _ => Nil
        }
        acc ++ segments
      }
      .withDefaultValue(Air)
  }

  override protected[year2022] def part1(input: Grid[Tile]): Int = {
    val maxDepth = input.keys.map(_.x).max
    settle(input, maxDepth).values.count(_ == Sand)
  }

  override protected[year2022] def part2(input: Grid[Tile]): Int = {
    val maxDepth = input.keys.map(_.x).max + 2
    // 0 to 1000 is arbitrary. Really only needs to be |maxDepth| in either direction from 500
    val floor = (0 to 1000).map(Point(maxDepth, _) -> Rock)
    settle(input ++ floor, maxDepth).values.count(_ == Sand)
  }

  private def settle(grid: Grid[Tile], maxDepth: Int, sandPosition: Point = Point(0, 500)): Grid[Tile] = {
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

    if (sandPosition.x > maxDepth) {
      grid
    } else {
      grid(sandPosition) match {
        case Sand | Rock => grid
        case Air         => helper(grid, List(sandPosition.down, sandPosition.down.left, sandPosition.down.right))
      }
    }
  }
}

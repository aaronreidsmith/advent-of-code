package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{Grid, Point, Solution, using}

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/a6wpup/2018_day_17_solutions/ebyws5k/
object Day17 extends Solution(2018, 17) {
  type I  = Grid[Tile]
  type O1 = Int
  type O2 = Int

  private[year2018] sealed trait Tile
  private[year2018] sealed trait WaterTile extends Tile

  private[year2018] case object Sand     extends Tile
  private[year2018] case object Clay     extends Tile
  private[year2018] case object Flowing  extends WaterTile
  private[year2018] case object Settling extends WaterTile
  private[year2018] case object Settled  extends WaterTile

  // The input use x increasing to the right and y increasing downward, but my `Point` class uses x increasing downward
  // and y increasing to the right, so I had to swap them (made this very confusing)
  override protected[year2018] def parseInput(file: Source): Grid[Tile] = {
    val xEntry = """^x=(\d+), y=(\d+)..(\d+)$""".r
    val yEntry = """^y=(\d+), x=(\d+)..(\d+)$""".r
    file.getLines().foldLeft(Map.empty[Point, Tile].withDefaultValue(Sand)) {
      case (acc, xEntry(x, yStart, yEnd)) => acc ++ (yStart.toInt to yEnd.toInt).map(Point(_, x.toInt) -> Clay).toMap
      case (acc, yEntry(y, xStart, xEnd)) => acc ++ (xStart.toInt to xEnd.toInt).map(Point(y.toInt, _) -> Clay).toMap
      case (acc, _)                       => acc
    }
  }

  override protected[year2018] def part1(tiles: Grid[Tile]): Int = {
    val depths   = tiles.keys.map(_.x)
    val minDepth = depths.min
    val maxDepth = depths.max
    flood(tiles, maxDepth, Point(0, 500), Point(-1, 500)).count {
      case (pos, _: WaterTile) => pos.x >= minDepth
      case _                   => false
    }
  }

  override protected[year2018] def part2(tiles: Grid[Tile]): Int = {
    val depths   = tiles.keys.map(_.x)
    val minDepth = depths.min
    val maxDepth = depths.max
    flood(tiles, maxDepth, Point(0, 500), Point(-1, 500)).count {
      case (pos, Settled) => pos.y >= minDepth
      case _              => false
    }
  }

  private def flood(tiles: Grid[Tile], maxDepth: Int, position: Point, previousPosition: Point): Grid[Tile] = {
    if (position.x > maxDepth) {
      tiles
    } else {
      tiles(position) match {
        case Clay | Flowing | Settled | Settling => tiles
        case Sand =>
          val down      = position.down
          val downTiles = flood(tiles + (position -> Flowing), maxDepth, down, position)
          downTiles(down) match {
            case Flowing | Settling => downTiles
            case Clay | Settled =>
              val left       = position.left
              val leftTiles  = flood(downTiles, maxDepth, left, position)
              val right      = position.right
              val rightTiles = flood(leftTiles, maxDepth, right, position)
              (rightTiles(left), rightTiles(right)) match {
                case (Clay | Settled | Settling, _) | (_, Clay | Settled | Settling)
                    if previousPosition == left || previousPosition == right =>
                  rightTiles + (position -> Settling)
                case (Clay | Settled | Settling, Clay | Settled | Settling) =>
                  settle(settle(rightTiles, left), right) + (position -> Settled)
                case _ => rightTiles
              }
            case Sand => downTiles
          }
      }
    }
  }

  private def settle(tiles: Grid[Tile], pos: Point): Grid[Tile] = {
    tiles(pos) match {
      case Settling => settle(settle(tiles + (pos -> Settled), pos.left), pos.right)
      case _        => tiles
    }
  }
}

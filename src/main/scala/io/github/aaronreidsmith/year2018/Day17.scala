package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/a6wpup/2018_day_17_solutions/ebyws5k/
object Day17 {
  private[year2018] type Pos = (Int, Int)
  private[year2018] implicit class PosOps(pos: Pos) {
    val (x, y) = pos
  }

  private[year2018] type Tiles = Map[Pos, Tile]
  private[year2018] object Tiles {
    def empty: Tiles = Map.empty[Pos, Tile].withDefaultValue(Sand)
  }

  private[year2018] sealed trait Tile
  private[year2018] sealed trait WaterTile extends Tile

  private[year2018] case object Sand     extends Tile
  private[year2018] case object Clay     extends Tile
  private[year2018] case object Flowing  extends WaterTile
  private[year2018] case object Settling extends WaterTile
  private[year2018] case object Settled  extends WaterTile

  private val xEntry = "^x=(\\d+), y=(\\d+)..(\\d+)$".r
  private val yEntry = "^y=(\\d+), x=(\\d+)..(\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = using("2018/day17.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2018] def parseInput(file: Source): Tiles = file.getLines().foldLeft(Tiles.empty) { (acc, line) =>
    val newEntries = line match {
      case xEntry(x, yStart, yEnd) =>
        (yStart.toInt to yEnd.toInt).foldLeft(Map.empty[(Int, Int), Tile]) { (clayAcc, y) =>
          clayAcc.updated((x.toInt, y), Clay)
        }
      case yEntry(y, xStart, xEnd) =>
        (xStart.toInt to xEnd.toInt).foldLeft(Map.empty[(Int, Int), Tile]) { (clayAcc, x) =>
          clayAcc.updated((x, y.toInt), Clay)
        }
      case _ => throw new IllegalArgumentException
    }
    acc ++ newEntries
  }

  private[year2018] def part1(tiles: Tiles): Int = {
    val ys   = tiles.keys.map(_.y)
    val minY = ys.min
    val maxY = ys.max
    flood(tiles, maxY, (500, 0), (500, -1)).count {
      case (pos, tile) => pos.y >= minY && tile.isInstanceOf[WaterTile]
    }
  }

  private[year2018] def part2(tiles: Tiles): Int = {
    val ys   = tiles.keys.map(_.y)
    val minY = ys.min
    val maxY = ys.max
    flood(tiles, maxY, (500, 0), (500, -1)).count {
      case (pos, tile) => pos.y >= minY && tile == Settled
    }
  }

  private def flood(tiles: Tiles, maxY: Int, pos: Pos, previousPos: Pos): Tiles = {
    if (pos.y > maxY) {
      tiles
    } else {
      tiles(pos) match {
        case Clay | Flowing | Settled | Settling => tiles
        case Sand =>
          val downPos   = (pos.x, pos.y + 1)
          val downTiles = flood(tiles + (pos -> Flowing), maxY, downPos, pos)
          downTiles(downPos) match {
            case Flowing | Settling => downTiles
            case Clay | Settled =>
              val leftPos    = (pos.x - 1, pos.y)
              val leftTiles  = flood(downTiles, maxY, leftPos, pos)
              val rightPos   = (pos.x + 1, pos.y)
              val rightTiles = flood(leftTiles, maxY, rightPos, pos)
              (rightTiles(leftPos), rightTiles(rightPos)) match {
                case (Clay | Settled | Settling, _) | (_, Clay | Settled | Settling)
                    if previousPos == leftPos || previousPos == rightPos =>
                  rightTiles + (pos -> Settling)
                case (Clay | Settled | Settling, Clay | Settled | Settling) =>
                  settle(settle(rightTiles, leftPos), rightPos) + (pos -> Settled)
                case _ => rightTiles
              }
            case Sand => downTiles
          }
      }
    }
  }

  private def settle(tiles: Tiles, pos: Pos): Tiles = {
    tiles(pos) match {
      case Settling => settle(settle(tiles + (pos -> Settled), (pos.x - 1, pos.y)), (pos.x + 1, pos.y))
      case _        => tiles
    }
  }
}

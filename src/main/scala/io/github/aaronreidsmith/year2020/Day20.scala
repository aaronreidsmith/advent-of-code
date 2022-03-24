package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/kgo01p/2020_day_20_solutions/ggkdioo/
object Day20 {
  private[year2020] object Orientation extends Enumeration {
    type Orientation = Value
    val Normal, Normal90, Normal180, Normal270, Flipped, Flipped90, Flipped180, Flipped270 = Value
  }
  import Orientation._

  private[year2020] case class TileData(lines: Vector[String]) {
    def width: Int  = lines.head.length
    def height: Int = lines.length

    // Find the edges
    def top: String    = lines.head
    def bottom: String = lines.last.reverse
    def right: String  = lines.map(_.last).mkString
    def left: String   = lines.map(_.head).reverse.mkString

    def flipVertically: TileData = this.copy(lines.map(_.reverse))

    def allEdges: Vector[String] = {
      def flipped = flipVertically
      Vector(top, right, bottom, left, flipped.top, flipped.right, flipped.bottom, flipped.left)
    }

    def orient(orientation: Orientation): TileData = orientation match {
      case Normal     => this
      case Normal90   => rot90
      case Normal180  => rot90.rot90
      case Normal270  => rot90.rot90.rot90
      case Flipped    => flipVertically
      case Flipped90  => flipVertically.rot90
      case Flipped180 => flipVertically.rot90.rot90
      case Flipped270 => flipVertically.rot90.rot90.rot90
    }

    def rot90: TileData = {
      val updated = lines.head.indices.map { col =>
        lines.indices
          .map { row =>
            lines(row)(col)
          }
          .reverse
          .mkString
      }.toVector
      TileData(updated)
    }

    def snipEdges: TileData = this.copy(lines.init.tail.map(_.init.tail))

    def at(x: Int, y: Int): Char = lines(y)(x)

    def set(x: Int, y: Int, newChar: Char): TileData = {
      val updated = lines.zipWithIndex.map {
        case (row, y2) =>
          row.zipWithIndex.map {
            case (char, x2) => if (x == x2 && y == y2) newChar else char
          }.mkString
      }
      TileData(updated)
    }
  }

  private case class OrientedTile(tileId: Int, orientation: Orientation)

  def main(args: Array[String]): Unit = {
    val input = using("2020/day20.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2020] def parseInput(file: Source): Map[Int, TileData] = {
    file.mkString.split("\n\n").foldLeft(Map.empty[Int, TileData]) { (acc, block) =>
      val lines  = block.split("\n")
      val tileId = lines.head.filter(_.isDigit).toInt
      acc.updated(tileId, TileData(lines.tail.toVector))
    }
  }

  private[year2020] def part1(input: Map[Int, TileData]): Long = {
    val arranged = arrangeTiles(input)
    Seq(arranged.head.head, arranged.head.last, arranged.last.head, arranged.last.last).foldLeft(1L)(_ * _.tileId)
  }

  private[year2020] def part2(input: Map[Int, TileData]): Int = {
    val arranged = arrangeTiles(input)
    val merged   = mergeArrangement(input, arranged)
    val monster = TileData(
      Vector(
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      )
    )
    val monsters = Orientation.values.toVector.map(monster.orient)

    def subtractMonsterEverywhereAndCountHashes(monster: TileData): Int = {
      val monsterPositions = for {
        x <- 0 until (merged.width - monster.width)
        y <- 0 until (merged.height - monster.height)
      } yield (x, y)

      def subtractMonsterIfThere(data: TileData, x: Int, y: Int): TileData = {
        val pixels = for {
          xm <- 0 until monster.width
          ym <- 0 until monster.height
          if monster.at(xm, ym) == '#'
        } yield data.at(x + xm, y + ym)

        if (pixels.forall(_ == '#')) {
          val monsterPixelPositions = for {
            xm <- 0 until monster.width
            ym <- 0 until monster.height
            if monster.at(xm, ym) == '#'
          } yield (xm, ym)

          monsterPixelPositions.foldLeft(data) {
            case (acc, (xm, ym)) => acc.set(x + xm, y + ym, 'O')
          }
        } else {
          data
        }
      }

      val withoutMonsters = monsterPositions.foldLeft(merged) {
        case (acc, (x, y)) => subtractMonsterIfThere(acc, x, y)
      }

      withoutMonsters.lines.foldLeft(0)(_ + _.count(_ == '#'))
    }

    monsters.map(subtractMonsterEverywhereAndCountHashes).min
  }

  private def arrangeTiles(tiles: Map[Int, TileData]): Vector[Vector[OrientedTile]] = {
    val topLeftCorner = {
      val allEdges = tiles.values.flatMap(_.allEdges)
      tiles.collectFirst {
        case (tileId, tileData) if allEdges.count(_ == tileData.top) == 1 && allEdges.count(_ == tileData.left) == 1 =>
          OrientedTile(tileId, Normal)
      }.get
    }
    val size = math.sqrt(tiles.size).round.toInt

    def allExceptCurrent(tile: OrientedTile): List[OrientedTile] = for {
      orientation <- Orientation.values.toList
      tileId      <- tiles.keys
      if tileId != tile.tileId
    } yield OrientedTile(tileId, orientation)

    def project(tile: OrientedTile): TileData = {
      tiles(tile.tileId).orient(tile.orientation)
    }

    def edgesMatchHorizontally(left: TileData, right: TileData): Boolean = {
      left.right == right.left.reverse
    }

    def edgesMatchVertically(top: TileData, bottom: TileData): Boolean = {
      top.bottom == bottom.top.reverse
    }

    def matchesHorizontally(left: OrientedTile, right: OrientedTile): Boolean = {
      edgesMatchHorizontally(project(left), project(right))
    }

    def matchesVertically(top: OrientedTile, bottom: OrientedTile): Boolean = {
      edgesMatchVertically(project(top), project(bottom))
    }

    def findMatchingTileHorizontally(tile: OrientedTile): OrientedTile = {
      allExceptCurrent(tile).find(matchesHorizontally(tile, _)).get
    }

    @tailrec
    def fillRowHorizontally(row: Vector[OrientedTile]): Vector[OrientedTile] = {
      if (row.size == size) row else fillRowHorizontally(row ++ Vector(findMatchingTileHorizontally(row.last)))
    }

    def findMatchingTileVertically(tile: OrientedTile): OrientedTile = {
      allExceptCurrent(tile).find(matchesVertically(tile, _)).get
    }

    def findMatchingRow(row: Vector[OrientedTile]): Vector[OrientedTile] = {
      row.map(findMatchingTileVertically)
    }

    @tailrec
    def fillRowsVertically(rows: Vector[Vector[OrientedTile]]): Vector[Vector[OrientedTile]] = {
      if (rows.size == size) rows else fillRowsVertically(rows ++ Vector(findMatchingRow(rows.last)))
    }

    val topRow = fillRowHorizontally(Vector(topLeftCorner))
    fillRowsVertically(Vector(topRow))
  }

  private def mergeArrangement(tiles: Map[Int, TileData], arrangement: Vector[Vector[OrientedTile]]): TileData = {
    val allTileData = arrangement.map { row =>
      row.map(tile => tiles(tile.tileId).orient(tile.orientation).snipEdges)
    }

    def mergeRow(row: Vector[TileData]): Vector[String] = {
      def mergeHorizontally(left: Vector[String], right: Vector[String]): Vector[String] = {
        left.zip(right).map { case (a, b) => a + b }
      }

      row.map(_.lines).reduceLeft(mergeHorizontally)
    }

    TileData(allTileData.flatMap(mergeRow))
  }
}

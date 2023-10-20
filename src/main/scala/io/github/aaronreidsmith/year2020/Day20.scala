package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/kgo01p/2020_day_20_solutions/ggkdioo/
object Day20 extends Solution {
  type I  = Map[Int, TileData]
  type O1 = Long
  type O2 = Int

  enum Orientation {
    case Normal, Normal90, Normal180, Normal270, Flipped, Flipped90, Flipped180, Flipped270
  }

  case class TileData(lines: Vector[String]) {
    def width: Int  = lines.head.length
    def height: Int = lines.length

    // Find the edges
    def top: String    = lines.head
    def bottom: String = lines.last.reverse
    def right: String  = lines.map(_.last).mkString
    def left: String   = lines.map(_.head).reverse.mkString

    def flipVertically: TileData = this.copy(lines.map(_.reverse))

    def allEdges: Vector[String] = {
      def flipped: TileData = flipVertically
      Vector(top, right, bottom, left, flipped.top, flipped.right, flipped.bottom, flipped.left)
    }

    def orient(orientation: Orientation): TileData = orientation match {
      case Orientation.Normal     => this
      case Orientation.Normal90   => rot90
      case Orientation.Normal180  => rot90.rot90
      case Orientation.Normal270  => rot90.rot90.rot90
      case Orientation.Flipped    => flipVertically
      case Orientation.Flipped90  => flipVertically.rot90
      case Orientation.Flipped180 => flipVertically.rot90.rot90
      case Orientation.Flipped270 => flipVertically.rot90.rot90.rot90
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

  case class OrientedTile(tileId: Int, orientation: Orientation)

  override def parseInput(file: Source): Map[Int, TileData] = {
    file.mkString.trim
      .split("\n\n")
      .foldLeft(Map.empty[Int, TileData]) { (acc, block) =>
        val lines  = block.split("\n")
        val tileId = lines.head.filter(_.isDigit).toInt
        acc.updated(tileId, TileData(lines.tail.toVector))
      }
  }

  override def part1(input: Map[Int, TileData]): Long = {
    val arranged = arrangeTiles(input)
    Seq(arranged.head.head, arranged.head.last, arranged.last.head, arranged.last.last).foldLeft(1L)(_ * _.tileId)
  }

  override def part2(input: Map[Int, TileData]): Int = {
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
          OrientedTile(tileId, Orientation.Normal)
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
        left.zip(right).map((a, b) => a + b)
      }

      row.map(_.lines).reduceLeft(mergeHorizontally)
    }

    TileData(allTileData.flatMap(mergeRow))
  }
}

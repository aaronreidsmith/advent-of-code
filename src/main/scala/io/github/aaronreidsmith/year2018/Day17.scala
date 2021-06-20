package io.github.aaronreidsmith.year2018

import scala.io.Source

object Day17 {
  sealed trait Tile
  sealed trait WaterTile extends Tile

  case object Sand     extends Tile      { override def toString: String = "." }
  case object Clay     extends Tile      { override def toString: String = "#" }
  case object Flowing  extends WaterTile { override def toString: String = "|" }
  case object Settling extends WaterTile { override def toString: String = "/" }
  case object Settled  extends WaterTile { override def toString: String = "~" }

  private val xEntry = "^x=(\\d+), y=(\\d+)..(\\d+)$".r("x", "yStart", "yEnd")
  private val yEntry = "^y=(\\d+), x=(\\d+)..(\\d+)$".r("y", "xStart", "xEnd")

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2018/day17.txt")
    // Input uses X for col and Y for row
    val tiles = input.getLines().foldLeft(Map.empty[(Int, Int), Tile].withDefaultValue(Sand)) { (acc, line) =>
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
    input.close()
    println(tiles)
  }
}

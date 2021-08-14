package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source

object Day18 {
  protected[this] case class Tile(isSafe: Boolean) {
    val isTrap: Boolean = !isSafe
  }

  implicit class CharOps(char: Char) {
    def toTile: Tile = if (char == '.') Tile(true) else Tile(false)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2016/day18.txt")
    val firstRow = input.getLines().foldLeft(Vector.empty[Vector[Tile]]) { (acc, line) =>
      acc :+ line.toCharArray.map(_.toTile).toVector
    }
    input.close()

    println(s"Part 1: ${solution(firstRow, 40)}")
    println(s"Part 2: ${solution(firstRow, 400000)}")
  }

  @tailrec
  def solution(tiles: Vector[Vector[Tile]], threshold: Int): Int = if (tiles.length >= threshold) {
    tiles.foldLeft(0)((acc, row) => acc + row.count(_.isSafe))
  } else {
    val lastRow = Tile(true) +: tiles.last :+ Tile(true) // add out "safe" tiles on each end
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
    }
    solution(tiles :+ newRow, threshold)
  }
}

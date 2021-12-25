package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25 {
  private implicit class Tuple2IntOps(point: (Int, Int)) {
    val (x, y) = point
  }
  private case class Grid(width: Int, height: Int, seaCucumbers: Map[(Int, Int), Char]) {
    def next: Grid = {
      val withEastMoved = seaCucumbers.map {
        case (point, char) =>
          val nextPoint = ((point.x + 1) % width, point.y)
          if (char == '>' && !seaCucumbers.contains(nextPoint)) nextPoint -> char else point -> char
      }
      val withSouthMoved = withEastMoved.map {
        case (point, char) =>
          val nextPoint = (point.x, (point.y + 1) % height)
          if (char == 'v' && !withEastMoved.contains(nextPoint)) nextPoint -> char else point -> char
      }
      this.copy(seaCucumbers = withSouthMoved)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Using
      .resource(Source.fromResource("2021/day25.txt")) { file =>
        val lines  = file.mkString.split('\n')
        val width  = lines.head.length
        val height = lines.length
        val points = {
          for {
            x <- 0 until width
            y <- 0 until height
            char = lines(y)(x)
            if char != '.'
          } yield (x, y) -> char
        }.toMap

        Grid(width, height, points)
      }

    println(s"Part 1: ${solution(input)}")
  }

  @tailrec
  private def solution(grid: Grid, steps: Int = 1): Int = {
    val next = grid.next
    if (next == grid) steps else solution(next, steps + 1)
  }
}

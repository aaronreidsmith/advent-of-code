package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends Solution {
  type I  = State
  type O1 = Int
  type O2 = Nothing

  case class State(width: Int, height: Int, seaCucumbers: Grid[Char]) {
    def next: State = {
      val withEastMoved = seaCucumbers.map {
        case (point, char) =>
          val nextPoint = point.copy(x = (point.x + 1) % width)
          if (char == '>' && !seaCucumbers.contains(nextPoint)) nextPoint -> char else point -> char
      }
      val withSouthMoved = withEastMoved.map {
        case (point, char) =>
          val nextPoint = point.copy(y = (point.y + 1) % height)
          if (char == 'v' && !withEastMoved.contains(nextPoint)) nextPoint -> char else point -> char
      }
      this.copy(seaCucumbers = withSouthMoved)
    }
  }

  override def parseInput(file: Source): State = {
    val lines  = file.mkString.split('\n')
    val width  = lines.head.length
    val height = lines.length
    val points = {
      for {
        x <- 0 until width
        y <- 0 until height
        char = lines(y)(x)
        if char != '.'
      } yield Point(x, y) -> char
    }.toMap

    State(width, height, points)
  }

  override def part1(input: State): Int = {
    @tailrec
    def helper(grid: State, steps: Int = 1): Int = {
      val next = grid.next
      if (next == grid) steps else helper(next, steps + 1)
    }
    helper(input)
  }
}

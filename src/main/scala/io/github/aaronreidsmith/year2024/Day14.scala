package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends Solution {
  type I  = List[Robot]
  type O1 = Int
  type O2 = Int

  case class Robot(position: Point, velocity: (Int, Int)) {
    def move: Robot = {
      val moved = position + velocity
      this.copy(position = Point(moved.x.mod(width), moved.y.mod(height)))
    }
  }

  private val robotRegex = """^p=(\d+),(\d+) v=(-?\d+),(-?\d+)$""".r
  private val width      = if (isTest) 11 else 101
  private val height     = if (isTest) 7 else 103

  override def parseInput(file: Source): List[Robot] = {
    file.getLines().toList.collect {
      case robotRegex(x, y, dx, dy) => Robot(Point(x.toInt, y.toInt), (dx.toInt, dy.toInt))
    }
  }

  override def part1(input: List[Robot]): Int = {
    @tailrec
    def helper(state: List[Robot], i: Int = 0): List[Robot] = {
      if (i >= 100) {
        state
      } else {
        helper(state.map(_.move), i + 1)
      }
    }

    val finalState = helper(input)
    val (q1, q2, q3, q4) = finalState.foldLeft((0, 0, 0, 0)) {
      case ((q1acc, q2acc, q3acc, q4acc), robot) =>
        (
          q1acc + (if (robot.position.x < width / 2 && robot.position.y < height / 2) 1 else 0),
          q2acc + (if (robot.position.x > width / 2 && robot.position.y < height / 2) 1 else 0),
          q3acc + (if (robot.position.x < width / 2 && robot.position.y > height / 2) 1 else 0),
          q4acc + (if (robot.position.x > width / 2 && robot.position.y > height / 2) 1 else 0)
        )
    }
    q1 * q2 * q3 * q4
  }

  override def part2(input: List[Robot]): Int = {
    @tailrec
    def helper(state: List[Robot], i: Int = 0): Int = {
      if (i > 0 && state.distinctBy(_.position).length == input.length) {
        i
      } else {
        helper(state.map(_.move), i + 1)
      }
    }

    helper(input)
  }
}

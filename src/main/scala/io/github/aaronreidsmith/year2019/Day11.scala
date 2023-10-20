package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.*
import io.github.aaronreidsmith.year2019.IntCode.State

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Solution {
  type I  = IntCode
  type O1 = Int
  type O2 = String

  override def parseInput(file: Source): IntCode = IntCode(file)
  override def part1(input: IntCode): Int        = paintHull(input).size
  override def part2(input: IntCode): String = {
    val paint    = paintHull(input, panels = Map(Point.zero -> 1L).withDefaultValue(0L))
    val (xs, ys) = paint.keys.unzip(point => (point.x, point.y))
    val output   = new StringBuilder("\n")
    (xs.min to xs.max).foreach { x =>
      (ys.min to ys.max).foreach { y =>
        val char = if (paint(Point(x, y)) == 1L) '#' else ' '
        output.addOne(char)
      }
      output.addOne('\n')
    }

    output.result()
  }

  @tailrec
  private def paintHull(
      intCode: IntCode,
      position: Point = Point.zero,
      direction: Direction = Direction.North,
      panels: Grid[Long] = Map(Point.zero -> 0L).withDefaultValue(0L)
  ): Grid[Long] = {
    val first = intCode.withInput(panels(position)).nextOutput
    first.result match {
      case State.Output(color) =>
        val second = first.nextOutput
        second.result match {
          case State.Output(turn) =>
            val newDirection = if (turn == 0) direction.left else direction.right
            paintHull(second, position.move(newDirection), newDirection, panels.updated(position, color))
          case _ => throw new IllegalArgumentException
        }
      case _ => panels
    }
  }
}

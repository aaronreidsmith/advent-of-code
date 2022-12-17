package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends Solution(2022, 17) {
  type I  = State
  type O1 = Int
  type O2 = Long

  private val shapes = Map(
    0 -> Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    1 -> Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)),
    2 -> Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)),
    3 -> Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),
    4 -> Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1))
  )

  private[year2022] case class State(grid: Set[Point], wind: String, shapeKey: Int, windPointer: Int, height: Int) {
    def next: State = {
      val shape                        = shapes(shapeKey).map(_ + Point(3, height + 4))
      val (nextWindPointer, nextShape) = fall(grid, shape, wind, windPointer)
      State(
        grid ++ nextShape,
        wind,
        (shapeKey + 1) % shapes.size,
        nextWindPointer,
        height.max(nextShape.map(_.y).max)
      )
    }
  }

  override protected[year2022] def parseInput(file: Source): State = {
    val wind = file.mkString.trim
    State(Set.tabulate(8)(Point(_, 0)), wind, 0, 0, 0)
  }

  override protected[year2022] def part1(input: State): Int = tetris(input).drop(2022).next().height

  override protected[year2022] def part2(input: State): Long = {
    val guess       = 1000
    val height      = tetris(input).slice(1, 10 * guess + 1).map(_.height).toSeq
    val delta       = height.sliding(2).map { case Seq(a, b) => b - a }.toSeq
    val index       = delta.lastIndexOfSlice(delta.takeRight(guess), delta.size - guess - 1)
    val cycleHeight = height(delta.size - guess) - height(index)
    val cycleWidth  = delta.size - guess - index
    val offset      = 1000000000000L - 1 - index
    val quotient    = offset / cycleWidth
    val remainder   = offset % cycleWidth
    (quotient * cycleHeight) + height(index + remainder.toInt)
  }

  @tailrec
  private def fall(grid: Set[Point], shape: Set[Point], wind: String, windPointer: Int): (Int, Set[Point]) = {
    var next = shape
    val gust = wind(windPointer)

    // Move left/right
    val shift = if (gust == '>') Point(1, 0) else Point(-1, 0)
    var test  = shape.map(_ + shift)
    if (test.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))) {
      next = test
    }

    // Move down
    test = next.map(_ + Point(0, -1))

    // Check if we're at the bottom
    if (test.exists(grid.contains)) {
      ((windPointer + 1) % wind.length, next)
    } else {
      fall(grid, test, wind, (windPointer + 1) % wind.length)
    }
  }

  private def tetris(initial: State): Iterator[State] = Iterator.iterate(initial)(_.next)
}

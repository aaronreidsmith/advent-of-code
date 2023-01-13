package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends Solution {
  type I  = State
  type O1 = Int
  type O2 = Long

  private val allShapes = Seq(
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),              // Horizontal
    Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)), // Plus
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)), // Backwards L
    Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),              // Vertical
    Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1))               // Square
  )

  case class State(
      wind: Iterator[Char],
      grid: Set[Point] = Set.tabulate(8)(Point(_, 0)),
      shapes: Iterator[Set[Point]] = Iterator.continually(allShapes).flatten,
      height: Int = 0
  ) {
    def next: State = {
      val shape     = shapes.next().map(_ + Point(3, height + 4))
      val nextShape = fall(grid, shape, wind)
      State(
        wind,
        grid ++ nextShape,
        shapes,
        height.max(nextShape.map(_.y).max)
      )
    }
  }

  override def parseInput(file: Source): State = {
    val wind = file.mkString.trim
    State(Iterator.continually(wind).flatten)
  }

  override def part1(input: State): Int = tetris(input).drop(2022).next().height

  override def part2(input: State): Long = {
    val guess       = 1000
    val height      = tetris(input).slice(1, 10 * guess + 1).map(_.height).toSeq
    val delta       = height.sliding(2).map(_.reduceRight(_ - _)).toSeq
    val index       = delta.lastIndexOfSlice(delta.takeRight(guess), delta.size - guess - 1)
    val cycleHeight = height(delta.size - guess) - height(index)
    val cycleWidth  = delta.size - guess - index
    val offset      = 1000000000000L - 1 - index
    val quotient    = offset / cycleWidth
    val remainder   = offset % cycleWidth
    (quotient * cycleHeight) + height(index + remainder.toInt)
  }

  @tailrec
  private def fall(grid: Set[Point], shape: Set[Point], wind: Iterator[Char]): Set[Point] = {
    var next = shape
    val gust = wind.next()

    // Move left/right
    val shift = if (gust == '>') Point(1, 0) else Point(-1, 0)
    var test  = shape.map(_ + shift)
    if (test.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))) {
      next = test
    }

    // Move down
    test = next.map(_ + Point(0, -1))

    // Check if we're at the bottom
    if (test.exists(grid.contains)) next else fall(grid, test, wind)
  }

  private def tetris(initial: State): Iterator[State] = Iterator.iterate(initial)(_.next)
}

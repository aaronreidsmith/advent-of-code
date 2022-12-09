package io.github

import scala.io.Source
import scala.util.Using

package object aaronreidsmith {
  case class Point(x: Int, y: Int) extends Ordered[Point] {
    def compare(that: Point): Int = {
      import scala.math.Ordered.orderingToOrdered
      (this.x, this.y).compare((that.x, that.y))
    }

    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def up: Point    = Point(x, y + 1)
    def right: Point = Point(x + 1, y)
    def down: Point  = Point(x, y - 1)
    def left: Point  = Point(x - 1, y)

    // Helpers because I always screw up comparisons
    def isAbove(that: Point): Boolean      = this.y > that.y
    def isBelow(that: Point): Boolean      = this.y < that.y
    def isLeftOf(that: Point): Boolean     = this.x < that.x
    def isRightOf(that: Point): Boolean    = this.x > that.x
    def sameColumnAs(that: Point): Boolean = this.x == that.x
    def sameRowAs(that: Point): Boolean    = this.y == that.y

    def immediateNeighbors: Seq[Point] = Seq(up, right, left, down)

    def neighbors: Seq[Point] = for {
      dx <- Seq(-1, 0, 1)
      dy <- Seq(-1, 0, 1)
      if (dx, dy) != (0, 0)
    } yield Point(x + dx, y + dy)

    def manhattanDistance(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs

    def unzip: (Int, Int) = (x, y)
  }

  object Point {
    def zero: Point = Point(0, 0)
  }

  type Grid[T] = Map[Point, T]

  def using[T](resourceName: String)(body: Source => T): T = Using.resource(Source.fromResource(resourceName))(body)
}

package io.github

import scala.io.Source
import scala.util.Using

package object aaronreidsmith {
  case class Point(x: Int, y: Int) {
    def neighbors: Seq[Point] = for {
      dx <- Seq(-1, 0, 1)
      dy <- Seq(-1, 0, 1)
      if (dx, dy) != (0, 0)
    } yield Point(x + dx, y + dy)

    def manhattanDistance(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs
  }

  type Grid[T] = Map[Point, T]

  def using[T](resourceName: String)(body: Source => T): T = Using.resource(Source.fromResource(resourceName))(body)
}

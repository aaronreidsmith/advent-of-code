package io.github.aaronreidsmith

case class Point(x: Int, y: Int) {
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def left: Point  = Point(x - 1, y)
  def right: Point = Point(x + 1, y)
  def up: Point    = Point(x, y - 1)
  def down: Point  = Point(x, y + 1)

  def immediateNeighbors: Seq[Point] = Seq(left, right, up, down)

  def neighbors: Seq[Point] = for {
    dx <- Seq(-1, 0, 1)
    dy <- Seq(-1, 0, 1)
    if (dx, dy) != (0, 0)
  } yield Point(x + dx, y + dy)

  def manhattanDistance(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs

  def unzip: (Int, Int) = (x, y)
}

object Point {
  def ZERO: Point = Point(0, 0)
}

package io.github.aaronreidsmith

case class Point(x: Int, y: Int) {
  infix def +(other: (Int, Int)): Point = Point(x + other._1, y + other._2)
  infix def +(other: Point): Point      = Point(x + other.x, y + other.y)

  infix def -(other: (Int, Int)): Point = Point(x - other._1, y - other._2)
  infix def -(other: Point): Point      = Point(x - other.x, y - other.y)

  infix def *(n: Int): Point = Point(x * n, y * n)

  def move(direction: Direction, steps: Int = 1): Point = direction match {
    case Direction.North => this.copy(x = x - steps)
    case Direction.East  => this.copy(y = y + steps)
    case Direction.South => this.copy(x = x + steps)
    case Direction.West  => this.copy(y = y - steps)
  }

  // This is different than a traditional graph to play nice with the Grid[T] type, which is based on indices
  def up: Point    = Point(x - 1, y)
  def right: Point = Point(x, y + 1)
  def down: Point  = Point(x + 1, y)
  def left: Point  = Point(x, y - 1)

  // Helpers because I always screw up comparisons
  def isAbove(that: Point): Boolean      = this.x < that.x
  def isBelow(that: Point): Boolean      = this.x > that.x
  def isLeftOf(that: Point): Boolean     = this.y < that.y
  def isRightOf(that: Point): Boolean    = this.y > that.y
  def sameColumnAs(that: Point): Boolean = this.y == that.y
  def sameRowAs(that: Point): Boolean    = this.x == that.x

  def immediateNeighbors: Seq[Point] = Seq(up, right, left, down)

  // I prefer the API without empty braces, so this is a workaround
  def neighbors: Seq[Point] = neighbors()
  def neighbors(includeSelf: Boolean = false): Seq[Point] = for {
    dx <- Seq(-1, 0, 1)
    dy <- Seq(-1, 0, 1)
    if (dx, dy) != (0, 0) || includeSelf
  } yield Point(x + dx, y + dy)

  def manhattanDistance(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs
}

object Point {
  given Ordering[Point] with {
    override def compare(left: Point, right: Point): Int = left.x.compareTo(right.x) match {
      case 0     => left.y.compareTo(right.y)
      case other => other
    }
  }
  given Conversion[Point, (Int, Int)] with {
    override def apply(point: Point): (Int, Int) = (point.x, point.y)
  }

  def zero: Point = Point(0, 0)
}

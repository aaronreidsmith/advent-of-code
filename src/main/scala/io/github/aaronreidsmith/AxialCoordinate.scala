package io.github.aaronreidsmith

sealed trait AxialCoordinate {
  type Self <: AxialCoordinate

  def q: Int
  def r: Int
  def neighbors: Seq[Self]

  def s: Int = -q - r
  def distanceFrom(that: Self): Int = {
    (math.abs(this.q - that.q) + math.abs(this.r - that.r) + math.abs(this.s - that.s)) / 2
  }
}

case class FlatCoordinate(q: Int, r: Int) extends AxialCoordinate {
  type Self = FlatCoordinate

  def north: FlatCoordinate     = FlatCoordinate(q, r - 1)
  def northEast: FlatCoordinate = FlatCoordinate(q + 1, r - 1)
  def southEast: FlatCoordinate = FlatCoordinate(q + 1, r)
  def south: FlatCoordinate     = FlatCoordinate(q, r + 1)
  def southWest: FlatCoordinate = FlatCoordinate(q - 1, r + 1)
  def northWest: FlatCoordinate = FlatCoordinate(q - 1, r)

  def neighbors: Seq[FlatCoordinate] = Seq(north, northEast, southEast, south, southWest, northWest)
}

object FlatCoordinate {
  def zero: FlatCoordinate = FlatCoordinate(0, 0)
}

case class PointyCoordinate(q: Int, r: Int) extends AxialCoordinate {
  type Self = PointyCoordinate

  def northEast: PointyCoordinate = PointyCoordinate(q + 1, r - 1)
  def east: PointyCoordinate      = PointyCoordinate(q + 1, r)
  def southEast: PointyCoordinate = PointyCoordinate(q, r + 1)
  def southWest: PointyCoordinate = PointyCoordinate(q - 1, r + 1)
  def west: PointyCoordinate      = PointyCoordinate(q - 1, r)
  def northWest: PointyCoordinate = PointyCoordinate(q, r - 1)

  def neighbors: Seq[PointyCoordinate] = Seq(northEast, east, southEast, southWest, west, northWest)
}

object PointyCoordinate {
  def zero: PointyCoordinate = PointyCoordinate(0, 0)
}

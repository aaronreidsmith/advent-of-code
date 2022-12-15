package io.github.aaronreidsmith

sealed trait Direction {
  def left: Direction
  def right: Direction
  def opposite: Direction
}
object Direction {
  def fromChar(char: Char): Direction = char.toUpper match {
    case 'N' | '^' => North
    case 'E' | '>' => East
    case 'S' | 'V' => South
    case 'W' | '<' => West
    case other     => throw new IllegalArgumentException(other.toString)
  }
}

case object North extends Direction {
  def left: Direction     = West
  def right: Direction    = East
  def opposite: Direction = South
}
case object East extends Direction {
  def left: Direction     = North
  def right: Direction    = South
  def opposite: Direction = West
}
case object South extends Direction {
  def left: Direction     = East
  def right: Direction    = West
  def opposite: Direction = North
}
case object West extends Direction {
  def left: Direction     = South
  def right: Direction    = North
  def opposite: Direction = East
}

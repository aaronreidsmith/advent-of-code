package io.github.aaronreidsmith

enum Direction {
  case North, East, South, West

  def left: Direction = this match {
    case North => West
    case East  => North
    case South => East
    case West  => South
  }
  
  def right: Direction = this match {
    case North => East
    case East  => South
    case South => West
    case West  => North
  }
  
  def opposite: Direction = this match {
    case North => South
    case East  => West
    case South => North
    case West  => East
  }
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

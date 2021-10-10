package io.github.aaronreidsmith.year2019.intcode.util

sealed trait ParameterMode
case object Position  extends ParameterMode
case object Immediate extends ParameterMode
case object Relative  extends ParameterMode

object ParameterMode {
  def apply(int: Int): ParameterMode = int match {
    case 0     => Position
    case 1     => Immediate
    case 2     => Relative
    case other => throw new IllegalArgumentException(s"Unknown parameter mode: $other")
  }
}

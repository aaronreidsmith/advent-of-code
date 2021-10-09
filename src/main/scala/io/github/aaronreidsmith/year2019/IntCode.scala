package io.github.aaronreidsmith.year2019

import scala.annotation.tailrec

trait IntCode {
  @tailrec
  final def applyIntcode(intcode: Vector[Int], pointer: Int = 0): Int = intcode(pointer) match {
    case oneOrTwo if oneOrTwo == 1 || oneOrTwo == 2 =>
      val int1        = intcode(intcode(pointer + 1))
      val int2        = intcode(intcode(pointer + 2))
      val targetIndex = intcode(pointer + 3)
      val updatedIntcode = if (oneOrTwo == 1) {
        intcode.updated(targetIndex, int1 + int2)
      } else {
        intcode.updated(targetIndex, int1 * int2)
      }
      applyIntcode(updatedIntcode, pointer + 4)
    case 99    => intcode.head
    case other => throw new IllegalArgumentException(s"$other is not a valid intcode")
  }
}

package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day01 extends Solution {
  type I  = Seq[Rotation]
  type O1 = Int
  type O2 = Int

  enum Direction {
    case Left, Right
  }

  case class Rotation(direction: Direction, amount: Int)

  override def parseInput(file: Source): Seq[Rotation] = {
    val right = """^R(\d+)$""".r
    val left  = """^L(\d+)$""".r
    file
      .getLines()
      .toSeq
      .collect {
        case right(amount) => Rotation(Direction.Right, amount.toInt)
        case left(amount)  => Rotation(Direction.Left, amount.toInt)
      }
  }

  override def part1(input: Seq[Rotation]): Int = {
    input
      .foldLeft((0, 50)) {
        case ((password, pointer), Rotation(direction, amountRaw)) =>
          val amount = amountRaw % 100
          val newPointer = direction match {
            case Direction.Left =>
              val raw = pointer - amount
              if (raw < 0) 100 + raw else raw
            case Direction.Right =>
              val raw = pointer + amount
              if (raw >= 100) raw % 100 else raw
          }
          val newPassword = if (newPointer == 0) password + 1 else password
          (newPassword, newPointer)
      }
      ._1
  }

  override def part2(input: Seq[Rotation]): Int = {
    input
      .flatMap {
        case Rotation(Direction.Left, amount)  => Seq.fill(amount)(-1)
        case Rotation(Direction.Right, amount) => Seq.fill(amount)(1)
      }
      .foldLeft((0, 50)) {
        case ((password, pointer), move) =>
          val newPointer = {
            val raw = pointer + move
            if (raw < 0) {
              100 + raw
            } else if (raw >= 100) {
              raw % 100
            } else {
              raw
            }
          }
          val newPassword = if (newPointer == 0) password + 1 else password
          (newPassword, newPointer)
      }
      ._1
  }
}

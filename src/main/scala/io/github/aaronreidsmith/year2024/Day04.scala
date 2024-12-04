package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Grid, Solution}

import scala.io.Source

object Day04 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  extension (b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = {
    input.foldLeft(0) {
      case (acc, (point, 'X')) =>
        val horizontalForward = {
          input.getOrElse(point.right, '.') == 'M' &&
          input.getOrElse(point.right.right, '.') == 'A' &&
          input.getOrElse(point.right.right.right, '.') == 'S'
        }
        val horizontalBackward = {
          input.getOrElse(point.left, '.') == 'M' &&
          input.getOrElse(point.left.left, '.') == 'A' &&
          input.getOrElse(point.left.left.left, '.') == 'S'
        }
        val verticalDown = {
          input.getOrElse(point.down, '.') == 'M' &&
          input.getOrElse(point.down.down, '.') == 'A' &&
          input.getOrElse(point.down.down.down, '.') == 'S'
        }
        val verticalUp = {
          input.getOrElse(point.up, '.') == 'M' &&
          input.getOrElse(point.up.up, '.') == 'A' &&
          input.getOrElse(point.up.up.up, '.') == 'S'
        }
        val diagonalUpLeft = {
          input.getOrElse(point.up.left, '.') == 'M' &&
          input.getOrElse(point.up.left.up.left, '.') == 'A' &&
          input.getOrElse(point.up.left.up.left.up.left, '.') == 'S'
        }
        val diagonalUpRight = {
          input.getOrElse(point.up.right, '.') == 'M' &&
          input.getOrElse(point.up.right.up.right, '.') == 'A' &&
          input.getOrElse(point.up.right.up.right.up.right, '.') == 'S'
        }
        val diagonalDownLeft = {
          input.getOrElse(point.down.left, '.') == 'M' &&
          input.getOrElse(point.down.left.down.left, '.') == 'A' &&
          input.getOrElse(point.down.left.down.left.down.left, '.') == 'S'
        }
        val diagonalDownRight = {
          input.getOrElse(point.down.right, '.') == 'M' &&
          input.getOrElse(point.down.right.down.right, '.') == 'A' &&
          input.getOrElse(point.down.right.down.right.down.right, '.') == 'S'
        }

        acc +
          horizontalForward.toInt +
          horizontalBackward.toInt +
          verticalUp.toInt +
          verticalDown.toInt +
          diagonalUpLeft.toInt +
          diagonalUpRight.toInt +
          diagonalDownLeft.toInt +
          diagonalDownRight.toInt
      case (acc, _) => acc
    }
  }

  override def part2(input: Grid[Char]): Int = {
    input.foldLeft(0) {
      case (acc, (point, 'A')) =>
        val backSlash = {
          (input.getOrElse(point.down.right, '.') == 'M' && input.getOrElse(point.up.left, '.') == 'S') ||
          (input.getOrElse(point.up.left, '.') == 'M' && input.getOrElse(point.down.right, '.') == 'S')
        }
        val forwardSlash = {
          (input.getOrElse(point.down.left, '.') == 'M' && input.getOrElse(point.up.right, '.') == 'S') ||
          (input.getOrElse(point.up.right, '.') == 'M' && input.getOrElse(point.down.left, '.') == 'S')
        }
        acc + (forwardSlash && backSlash).toInt
      case (acc, _) => acc
    }
  }
}

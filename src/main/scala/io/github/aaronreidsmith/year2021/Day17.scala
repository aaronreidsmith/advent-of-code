package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends Solution {
  type I  = IndexedSeq[Int]
  type O1 = Int
  type O2 = Int

  // parseInput just brute forces all the trajectories since there are so few
  override def parseInput(file: Source): IndexedSeq[Int] = {
    val targetArea                         = "^target area: x=(\\d+)..(\\d+), y=(-?\\d+)..(-?\\d+)".r
    val targetArea(xMin, xMax, yMin, yMax) = file.mkString.trim

    val xRange = xMin.toInt to xMax.toInt
    val yRange = yMin.toInt to yMax.toInt

    @tailrec
    def maxHeight(
        xVelocity: Int,
        yVelocity: Int,
        xPosition: Int = 0,
        yPosition: Int = 0,
        currentMaxHeight: Int = Int.MinValue
    ): Int = if (xVelocity <= 0 && !xRange.contains(xPosition)) { // Didn't make it far enough, or overshot
      Int.MinValue
    } else if (yPosition < yRange.min) { // Went too low
      Int.MinValue
    } else if (xRange.contains(xPosition) && yRange.contains(yPosition)) { // Hit the target
      currentMaxHeight
    } else { // Still going up or still coming down
      val newXPosition = xPosition + xVelocity
      val newYPosition = yPosition + yVelocity
      val newXVelocity = (xVelocity - 1).max(0)
      val newYVelocity = yVelocity - 1
      val newMaxHeight = currentMaxHeight.max(newYPosition)
      maxHeight(newXVelocity, newYVelocity, newXPosition, newYPosition, newMaxHeight)
    }

    for {
      xVelocity <- 0 to xRange.max
      yVelocity <- yRange.min to 1000
    } yield maxHeight(xVelocity, yVelocity)
  }

  override def part1(input: IndexedSeq[Int]): Int = input.max
  override def part2(input: IndexedSeq[Int]): Int = input.count(_ != Int.MinValue)
}

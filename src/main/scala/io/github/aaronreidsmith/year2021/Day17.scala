package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17 {
  private val targetArea = "^target area: x=(\\d+)..(\\d+), y=(-?\\d+)..(-?\\d+)".r

  def main(args: Array[String]): Unit = {
    val targetArea(xMin, xMax, yMin, yMax) = Using.resource(Source.fromResource("2021/day17.txt"))(_.mkString)

    val xRange = xMin.toInt to xMax.toInt
    val yRange = yMin.toInt to yMax.toInt

    @tailrec
    def maxHeight(
        xVelocity: Int,
        yVelocity: Int,
        xPosition: Int = 0,
        yPosition: Int = 0,
        seenHeights: Set[Int] = Set()
    ): Int = if (xVelocity <= 0 && !xRange.contains(xPosition)) { // Didn't make it far enough, or overshot
      Int.MinValue
    } else if (yPosition < yRange.min) { // Went too low
      Int.MinValue
    } else if (xRange.contains(xPosition) && yRange.contains(yPosition)) { // Hit the target
      seenHeights.max
    } else { // Still going up or still coming down
      val newXPosition = xPosition + xVelocity
      val newYPosition = yPosition + yVelocity
      val newXVelocity = (xVelocity - 1).max(0)
      val newYVelocity = yVelocity - 1
      maxHeight(newXVelocity, newYVelocity, newXPosition, newYPosition, seenHeights + newYPosition)
    }

    val maxHeights = for {
      xVelocity <- 0 to xRange.max
      yVelocity <- yRange.min to 1000
    } yield maxHeight(xVelocity, yVelocity)

    val part1 = maxHeights.max
    println(s"Part 1: $part1")

    val part2 = maxHeights.count(_ != Int.MinValue)
    println(s"Part 2: $part2")
  }
}

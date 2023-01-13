package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day11 extends Solution {
  type I  = Int
  type O1 = String
  type O2 = String

  // Since a fuel cell is just a point with some other methods, we just extend our Point class
  type FuelCell = Point
  object FuelCell {
    def apply(x: Int, y: Int): FuelCell = Point(x, y)
  }

  private implicit class FuelCellOps(fuelCell: FuelCell) {
    def powerLevel(gridSerialNumber: Int): Int = {
      val rackId         = fuelCell.x + 10
      val initPowerLevel = (rackId * fuelCell.y) + gridSerialNumber
      val powerLevel     = initPowerLevel * rackId
      val hundredsDigit  = if (powerLevel < 100) 0 else powerLevel.toString.reverse.charAt(2).asDigit
      hundredsDigit - 5
    }
  }

  override def parseInput(file: Source): Int = file.mkString.trim.toInt

  override def part1(input: Int): String = {
    val grid = for {
      x <- 1 to 300
      y <- 1 to 300
    } yield FuelCell(x, y)
    val maxSquareCenter = grid.maxBy(_.neighbors.foldLeft(0)(_ + _.powerLevel(input)))
    val topLeft         = FuelCell(maxSquareCenter.x - 1, maxSquareCenter.y - 1)
    s"${topLeft.x},${topLeft.y}"
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/a53r6i/2018_day_11_solutions/ebjogd7?utm_source=share&utm_medium=web2x&context=3
  override def part2(input: Int): String = {
    var bestX          = Int.MinValue
    var bestY          = Int.MinValue
    var bestSize       = Int.MinValue
    var bestPowerLevel = Int.MinValue
    val sum            = Array.fill(301)(Array.fill(301)(0))

    for {
      y <- 1 to 300
      x <- 1 to 300
    } {
      val id         = x + 10
      var powerLevel = id * y + input
      powerLevel = (powerLevel * id) / 100 % 10 - 5
      sum(y)(x) = powerLevel + sum(y - 1)(x) + sum(y)(x - 1) - sum(y - 1)(x - 1)
    }

    for {
      size <- 1 to 300
      y    <- size to 300
      x    <- size to 300
    } {
      val total = sum(y)(x) - sum(y - size)(x) - sum(y)(x - size) + sum(y - size)(x - size)
      if (total > bestPowerLevel) {
        bestPowerLevel = total
        bestX = x
        bestY = y
        bestSize = size
      }
    }
    s"${bestX - bestSize + 1},${bestY - bestSize + 1},$bestSize"
  }
}

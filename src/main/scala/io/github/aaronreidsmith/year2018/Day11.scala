package io.github.aaronreidsmith.year2018

object Day11 {
  case class FuelCell(x: Int, y: Int) {
    private val gridSerialNumber = 4455 // puzzle input
    lazy val powerLevel: Int = {
      val rackId         = x + 10
      val initPowerLevel = (rackId * y) + gridSerialNumber
      val powerLevel     = initPowerLevel * rackId
      val hundredsDigit  = if (powerLevel < 100) 0 else powerLevel.toString.reverse.charAt(2).asDigit
      hundredsDigit - 5
    }

    lazy val neighbors: List[FuelCell] = this :: List(
      FuelCell(x - 1, y),
      FuelCell(x + 1, y),
      FuelCell(x, y - 1),
      FuelCell(x, y + 1),
      FuelCell(x - 1, y - 1),
      FuelCell(x - 1, y + 1),
      FuelCell(x + 1, y - 1),
      FuelCell(x + 1, y + 1)
    ).filter(cell => cell.x >= 1 && cell.x <= 300 && cell.y >= 1 && cell.y <= 300)
  }

  def main(args: Array[String]): Unit = {
    val grid = for {
      y <- 1 to 300
      x <- 1 to 300
    } yield FuelCell(x, y)

    println(s"Part 1: ${part1(grid)}")
    println(s"Part 2: $part2")
  }

  def part1(grid: Seq[FuelCell]): String = {
    val maxSquareCenter = grid.maxBy(_.neighbors.foldLeft(0)(_ + _.powerLevel))
    val topLeft         = FuelCell(maxSquareCenter.x - 1, maxSquareCenter.y - 1)
    s"${topLeft.x},${topLeft.y}"
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/a53r6i/2018_day_11_solutions/ebjogd7?utm_source=share&utm_medium=web2x&context=3
  def part2: String = {
    var bestX          = Integer.MIN_VALUE
    var bestY          = Integer.MIN_VALUE
    var bestSize       = Integer.MIN_VALUE
    var bestPowerLevel = Integer.MIN_VALUE
    val sum            = Array.fill(301)(Array.fill(301)(0))

    for {
      y <- 1 to 300
      x <- 1 to 300
    } {
      val id         = x + 10
      var powerLevel = id * y + 4455 // puzzle input
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

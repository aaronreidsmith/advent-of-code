package io.github.aaronreidsmith.year2018

object Day22 {
  // Hardcoded rather than parsing input
  private val DEPTH  = 3879
  private val TARGET = (8, 713)
  private val ALL_REGIONS = {
    for {
      x <- 0 to TARGET._1
      y <- 0 to DEPTH
    } yield (x, y) -> Region(x, y)
  }.toMap

  private case class Region(x: Int, y: Int) {
    lazy val geologicalIndex: Int = (x, y) match {
      case (0, 0) | TARGET => 0
      case (_, 0)          => x * 16807
      case (0, _)          => y * 48271
      case (_, _)          => ALL_REGIONS((x - 1, y)).erosionLevel * ALL_REGIONS((x, y - 1)).erosionLevel
    }

    lazy val erosionLevel: Int = (geologicalIndex + DEPTH) % 20183
    lazy val riskLevel: Int    = erosionLevel              % 3
  }

  def main(args: Array[String]): Unit = {
    val (targetX, targetY) = TARGET
    val part1 = {
      for {
        x <- 0 to targetX
        y <- 0 to targetY
      } yield ALL_REGIONS((x, y)).riskLevel
    }.sum
    println(s"Part 1: $part1")
    // Part 2 in Python
  }
}

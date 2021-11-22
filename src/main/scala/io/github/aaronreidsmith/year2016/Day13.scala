package io.github.aaronreidsmith.year2016

object Day13 {
  private sealed trait Space
  private case object Open extends Space
  private case object Wall extends Space

  def main(args: Array[String]): Unit = {
    val input = 1350
    val grid = {
      for {
        x <- 0 to 50
        y <- 0 to 50
      } yield {
        val num      = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + input
        val oneCount = num.toBinaryString.count(_ == '1')
        (x, y) -> (if (oneCount % 2 == 0) Open else Wall)
      }
    }.toMap
    printGrid(grid) // Just printed and solved both parts by hand (Part 1: 92, Part 2: 124)
  }

  private def printGrid(grid: Map[(Int, Int), Space]): Unit = {
    var row = 0
    grid.toSeq
      .sortBy { case (position, _) => position }
      .foreach {
        case ((currRow, currCol), space) =>
          val char = space match {
            case Open => ' '
            case Wall => '#'
          }
          if (currRow != row) {
            row += 1
            println()
          }
          if ((currRow, currCol) == (31, 39)) {
            print('X')
          } else {
            print(char)
          }
      }
  }
}

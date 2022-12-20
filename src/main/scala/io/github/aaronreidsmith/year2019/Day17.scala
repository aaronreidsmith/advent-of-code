package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day17 extends Solution(2019, 17) {
  type I  = IntCode
  type O1 = Int
  type O2 = Long

  private implicit class PointOps(point: Point) {
    def isIntersection(map: Map[Point, Char]): Boolean = {
      val grid = map.withDefaultValue('.')
      grid(point) == '#' && point.immediateNeighbors.forall(grid(_) == '#')
    }
  }

  override protected[year2019] def parseInput(file: Source): IntCode = IntCode(file)
  override protected[year2019] def part1(input: IntCode): Int = {
    val grid = input.allOutput.foldLeft(List(List.empty[Char])) {
      case (acc, output) if output == 10L => acc :+ Nil
      case (acc, output)                  => acc.init :+ (acc.last :+ output.toChar)
    }

    // printGrid(grid)

    val map = {
      for {
        (rowList, row) <- grid.zipWithIndex
        (char, col)    <- rowList.zipWithIndex
      } yield Point(row, col) -> char
    }.toMap

    map.foldLeft(0) {
      case (acc, (point, _)) if point.isIntersection(map) => acc + (point.x * point.y)
      case (acc, _)                                       => acc
    }
  }

  override protected[year2019] def part2(input: IntCode): Long = {
    /* Did these all by hand based on output of `printGrid`
     *
     * Full routine: L,10,L,8,R,8,L,8,R,6,L,10,L,8,R,8,L,8,R,6,R,6,R,8,R,8,R,6,R,6,L,8,L,10,R,6,R,8,R,8,R,6,R,6,L,8,L,10,R,6,R,8,R,8,R,6,R,6,L,8,L,10,R,6,R,8,R,8,L,10,L,8,R,8,L,8,R,6
     *
     * Format:
     *   Main Routine
     *   Function A
     *   Function B
     *   Function C
     *   Continuous Video? (y/n)
     */
    val userInput = """A,A,B,C,B,C,B,C,B,A
                      |L,10,L,8,R,8,L,8,R,6
                      |R,6,R,8,R,8
                      |R,6,R,6,L,8,L,10
                      |n
                      |""".stripMargin.map(_.toLong)
    val updated = input.copy(memory = input.memory.updated(0L, 2L))
    updated.withInput(userInput: _*).allOutput.last
  }

  private def printGrid(grid: Seq[Seq[Char]]): Unit = {
    println(grid.map(_.mkString).mkString("\n"))
  }
}

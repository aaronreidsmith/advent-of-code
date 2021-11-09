package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

object Day17 extends IntCodeUtils {
  private case class Point(row: Int, col: Int) {
    private def left: Point  = this.copy(col = this.col - 1)
    private def right: Point = this.copy(col = this.col + 1)
    private def up: Point    = this.copy(row = this.row - 1)
    private def down: Point  = this.copy(row = this.row + 1)

    def isIntersection(map: Map[Point, Char]): Boolean = {
      map.getOrElse(this, '.') == '#' &&
      map.getOrElse(this.left, '.') == '#' &&
      map.getOrElse(this.right, '.') == '#' &&
      map.getOrElse(this.up, '.') == '#' &&
      map.getOrElse(this.down, '.') == '#'
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day17.txt")
    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: ${part2(instructions)}")
  }

  // Run this with `printGrid = true` to visualize the grid
  private def part1(instructions: Map[Long, Long], printGrid: Boolean = false): Int = {
    val intCode = new IntCode(instructions)
    val grid = intCode.run().getOutput.foldLeft(List(List.empty[Char])) {
      case (acc, output) if output == 10 => acc :+ Nil
      case (acc, output)                 => acc.init :+ (acc.last :+ output.toChar)
    }

    if (printGrid) println(grid.map(_.mkString).mkString("\n"))

    val map = {
      for {
        (rowList, row) <- grid.zipWithIndex
        (char, col)    <- rowList.zipWithIndex
      } yield Point(row, col) -> char
    }.toMap

    map.foldLeft(0) {
      case (acc, (point, _)) if point.isIntersection(map) => acc + (point.row * point.col)
      case (acc, _)                                       => acc
    }
  }

  private def part2(instructions: Map[Long, Long]): Long = {
    /* Did these all by hand based on output of `printGrid = true` in part 1
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
    val input =
      """A,A,B,C,B,C,B,C,B,A
        |L,10,L,8,R,8,L,8,R,6
        |R,6,R,8,R,8
        |R,6,R,6,L,8,L,10
        |n
        |""".stripMargin.map(_.toLong)

    new IntCode(instructions.updated(0L, 2L)).run(input).getOutput.last
  }
}

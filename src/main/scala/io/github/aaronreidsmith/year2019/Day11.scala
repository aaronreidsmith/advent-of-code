package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends IntCodeUtils {
  protected sealed trait Color
  protected case object Black extends Color
  protected case object White extends Color

  protected sealed trait Direction {
    val left: Direction
    val right: Direction
  }
  protected case object North extends Direction {
    val left: Direction  = West
    val right: Direction = East
  }
  protected case object East extends Direction {
    val left: Direction  = North
    val right: Direction = South
  }
  protected case object South extends Direction {
    val left: Direction  = East
    val right: Direction = West
  }
  protected case object West extends Direction {
    val left: Direction  = South
    val right: Direction = North
  }

  private class Robot(instructions: Map[Long, Long], startingColor: Color = Black) {
    private val intCode        = new IntCode(instructions, suspendOnOutput = true)
    private val hull           = mutable.Map((0, 0) -> startingColor).withDefaultValue(Black)
    private val paintedSquares = mutable.Set.empty[(Int, Int)]

    private var position             = (0, 0)
    private var direction: Direction = North

    def isFinished: Boolean = intCode.isFinished
    def squaresPainted: Int = paintedSquares.size
    def printGrid(): Unit = {
      // Find our boundaries
      val keys    = hull.keys.toSeq
      val numRows = keys.map(_._1).max + 1
      val numCols = keys.map(_._2).max + 1

      // Convert our map to an array and fill it in
      val grid = Array.fill(numRows)(Array.fill[Color](numCols)(Black))
      hull.foreach {
        case ((row, col), color) => grid(row)(col) = color
      }

      // Not entirely sure why I had to transpose here? But that is the only way it works
      grid.transpose.foreach { row =>
        row.foreach { square =>
          if (square == White) print("#") else print(" ")
        }
        println()
      }
    }

    def step(): Robot = {
      val currentColor = hull(position)
      val input        = if (currentColor == Black) 0 else 1

      // Paint our current square
      val firstOutput  = intCode.run(Seq(input)).getOutput.last
      val colorToPaint = if (firstOutput == 0) Black else White
      hull.update(position, colorToPaint)
      paintedSquares.add(position)

      // Turn our robot and step forward
      val secondOutput = intCode.run().getOutput.last
      direction = if (secondOutput == 0) direction.left else direction.right
      position = direction match {
        case North => (position._1, position._2 - 1)
        case East  => (position._1 + 1, position._2)
        case South => (position._1, position._2 + 1)
        case West  => (position._1 - 1, position._2)
      }

      this
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day11.txt")
    val part1Robot   = new Robot(instructions)
    println(s"Part 1: ${paintHull(part1Robot).squaresPainted}")

    val part2Robot = new Robot(instructions, startingColor = White)
    println(s"Part 2:")
    paintHull(part2Robot).printGrid()
  }

  @tailrec
  private def paintHull(robot: Robot): Robot = if (robot.isFinished) robot else paintHull(robot.step())
}

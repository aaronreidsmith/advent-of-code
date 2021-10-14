package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

import scala.annotation.tailrec
import scala.collection.mutable

object Day13 extends IntCodeUtils {
  private sealed trait Tile
  private case object Empty  extends Tile
  private case object Wall   extends Tile
  private case object Block  extends Tile
  private case object Paddle extends Tile
  private case object Ball   extends Tile

  private object Tile {
    def apply(id: Int): Tile = id match {
      case 0     => Empty
      case 1     => Wall
      case 2     => Block
      case 3     => Paddle
      case 4     => Ball
      case other => throw new IllegalArgumentException(s"$other is not a valid tile ID")
    }
  }

  private class Game(instructions: Map[Long, Long], initialGrid: Map[(Long, Long), Tile] = Map()) {
    private val intCode = new IntCode(instructions, suspendOnOutput = true)
    private val grid    = mutable.Map(initialGrid.toSeq: _*).withDefaultValue(Empty)

    private var score = 0L

    def blockCount: Int                  = grid.values.count(_ == Block)
    def getGrid: Map[(Long, Long), Tile] = grid.toMap

    def printGrid(): Unit = {
      // Find our boundaries
      val keys    = grid.keys.toSeq
      val numRows = keys.map(_._1).max.toInt + 1
      val numCols = keys.map(_._2).max.toInt + 1

      // Convert our map to an array and fill it in
      val outputGrid = Array.fill(numRows)(Array.fill[Tile](numCols)(Empty))
      grid.foreach {
        case ((row, col), tile) => outputGrid(row.toInt)(col.toInt) = tile
      }

      // Again, not super sure why we have to transpose here...
      outputGrid.transpose.foreach { row =>
        // Adapted from https://git.io/JKZRH
        row.foreach {
          case Empty  => print(" ")
          case Wall   => print(Character.toChars(9608).head)
          case Block  => print(Character.toChars(9632).head)
          case Paddle => print(Character.toChars(9620).head)
          case Ball   => print(Character.toChars(9675).head)
        }
        println()
      }
    }

    @tailrec
    final def drawBoard(): Game = if (intCode.isFinished) {
      this
    } else {
      val row  = intCode.run().getOutput.last
      val col  = intCode.run().getOutput.last
      val tile = Tile(intCode.run().getOutput.last.toInt)
      grid.update((row, col), tile)
      drawBoard()
    }

    // TODO: Don't make the user actually play the whole game...
    @tailrec
    final def play(maybeInput: Option[Long] = None): Long = if (intCode.isFinished) {
      score
    } else {
      // For some reason this prints several times between inputs, but idgaf
      printGrid()

      val row         = intCode.run(maybeInput.toSeq).getOutput.last
      val col         = intCode.run().getOutput.last
      val tileOrScore = intCode.run().getOutput.last

      if (row == -1 && col == 0) {
        score = tileOrScore
      } else {
        val tile = Tile(tileOrScore.toInt)
        grid.update((row, col), tile)
      }

      play()
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day13.txt")

    val part1 = new Game(instructions)
    println(s"Part 1: ${part1.drawBoard().blockCount}")

    val part2 = new Game(instructions ++ Map(0L -> 2L), initialGrid = part1.getGrid)
    println(s"Part 2: ${part2.play()}")
  }
}

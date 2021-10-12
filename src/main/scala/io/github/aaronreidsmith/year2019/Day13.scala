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

  private class Game(instructions: Map[Long, Long]) {
    private val intCode = new IntCode(instructions, suspendOnOutput = true)
    private val grid    = mutable.Map.empty[(Long, Long), Tile].withDefaultValue(Empty)

    def blocks: Int = grid.values.count(_ == Block)

    @tailrec
    final def play(): Game = if (intCode.isFinished) {
      this
    } else {
      val row  = intCode.run().getOutput.last
      val col  = intCode.run().getOutput.last
      val tile = Tile(intCode.run().getOutput.last.toInt)
      grid.update((row, col), tile)
      play()
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day13.txt")

    val part1        = new Game(instructions)
    println(s"Part 1: ${part1.play().blocks}")
  }
}

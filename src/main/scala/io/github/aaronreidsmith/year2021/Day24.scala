package io.github.aaronreidsmith.year2021

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

// Adapted from https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpsobsi
object Day24 {
  def main(args: Array[String]): Unit = {
    val blocks = Using.resource(Source.fromResource("2021/day24.txt")) { file =>
      file.mkString.split("inp w\n").tail.map(_.split('\n').toVector).toVector
    }
    val maxModel = mutable.IndexedBuffer.fill(14)(0)
    val minModel = mutable.IndexedBuffer.fill(14)(0)
    val stack    = mutable.Stack.empty[(Int, Int)]
    blocks.zipWithIndex.foreach {
      case (block, i) =>
        if (block(3) == "div z 1") {
          stack.prepend((i, block(14).split(' ').last.toInt)) // add y <val>
        } else if (block(3) == "div z 26") {
          val (j, x)                         = stack.pop()
          val diff                           = x + block(4).split(' ').last.toInt // add x <-val>
          val (targetI, targetJ, targetDiff) = if (diff < 0) (j, i, -diff) else (i, j, diff)
          maxModel(targetI) = 9
          maxModel(targetJ) = 9 - targetDiff
          minModel(targetI) = 1 + targetDiff
          minModel(targetJ) = 1
        }
    }
    println(s"Part 1: ${maxModel.mkString}")
    println(s"Part 2: ${minModel.mkString}")
  }
}

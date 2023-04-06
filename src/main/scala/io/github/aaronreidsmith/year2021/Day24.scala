package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpsobsi
object Day24 extends Solution {
  type I  = Vector[Vector[String]]
  type O1 = String
  type O2 = String

  override def parseInput(file: Source): Vector[Vector[String]] = {
    file.mkString.trim.split("inp w\n").tail.map(_.split('\n').toVector).toVector
  }

  override def part1(input: Vector[Vector[String]]): String = solution(input)._1
  override def part2(input: Vector[Vector[String]]): String = solution(input)._2

  private var part1Solution = ""
  private var part2Solution = ""
  private var solved        = false
  private def solution(input: Vector[Vector[String]]): (String, String) = {
    if (!solved) {
      val maxModel = mutable.IndexedBuffer.fill(14)(0)
      val minModel = mutable.IndexedBuffer.fill(14)(0)
      val stack    = mutable.Stack.empty[(Int, Int)]
      input.zipWithIndex.foreach {
        case (block, i) if block(3) == "div z 1" => stack.prepend((i, block(14).split(' ').last.toInt)) // add y <val>
        case (block, i) =>
          val (j, x)                         = stack.pop()
          val diff                           = x + block(4).split(' ').last.toInt // add x <-val>
          val (targetI, targetJ, targetDiff) = if (diff < 0) (j, i, -diff) else (i, j, diff)
          maxModel(targetI) = 9
          maxModel(targetJ) = 9 - targetDiff
          minModel(targetI) = 1 + targetDiff
          minModel(targetJ) = 1
      }
      part1Solution = maxModel.mkString
      part2Solution = minModel.mkString
      solved = true
    }

    (part1Solution, part2Solution)
  }
}

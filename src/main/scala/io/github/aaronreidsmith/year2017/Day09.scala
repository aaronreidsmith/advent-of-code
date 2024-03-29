package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): Int        = solution(input)._1
  override def part2(input: String): Int        = solution(input)._2

  // Both parts require the same traversal, so might as well only do it once
  private var part1Solution = 0
  private var part2Solution = 0
  private var solved        = false
  private def solution(input: String): (Int, Int) = {
    @tailrec
    def helper(
        stream: String,
        level: Int = 0,
        scores: Map[Int, Int] = Map(1 -> 0),
        garbageCount: Int = 0,
        inGarbage: Boolean = false,
        ignoreChar: Boolean = false
    ): Unit = stream.headOption match {
      case Some(char) =>
        val newStream = stream.drop(1)
        if (ignoreChar) {
          helper(newStream, level, scores, garbageCount, inGarbage)
        } else {
          char match {
            case '{' if !inGarbage => helper(newStream, level + 1, scores, garbageCount)
            case '}' if !inGarbage =>
              helper(newStream, level - 1, scores + (level -> (scores.getOrElse(level, 0) + 1)), garbageCount)
            case '<' if !inGarbage => helper(newStream, level, scores, garbageCount, inGarbage = true)
            case '>' if inGarbage  => helper(newStream, level, scores, garbageCount)
            case '!' if inGarbage  => helper(newStream, level, scores, garbageCount, inGarbage, ignoreChar = true)
            case _ =>
              val newGarbageCount = if (inGarbage) garbageCount + 1 else garbageCount
              helper(newStream, level, scores, newGarbageCount, inGarbage, ignoreChar)
          }
        }
      case None =>
        part1Solution = scores.foldLeft(0) { case (acc, (level, count)) => acc + (level * count) }
        part2Solution = garbageCount
        solved = true
    }

    if (!solved || isTest) {
      helper(input)
    }

    (part1Solution, part2Solution)
  }
}

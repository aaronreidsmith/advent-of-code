package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends Solution(2022, 10) {
  type I  = List[String]
  type O1 = Int
  type O2 = String

  override protected[year2022] def parseInput(file: Source): List[String] = file.getLines().toList
  override protected[year2022] def part1(input: List[String]): Int        = solution(input)._1
  override protected[year2022] def part2(input: List[String]): String     = solution(input)._2

  // Both parts require the same traversal, so might as well only do it once
  private var answer = (0, "")
  private var solved = false
  private def solution(input: List[String]): (Int, String) = {
    if (!solved) {
      val addX              = """^addx (-?\d+)$""".r
      val interestingCycles = Set(20, 60, 100, 140, 180, 220)

      // TODO: This could probably be cleaned up, but it works 🤷
      @tailrec
      def helper(
          instructions: List[String],
          x: Int = 1,
          cycle: Int = 1,
          crtPosition: Int = 0,
          part1Acc: Int = 0,
          part2Acc: StringBuilder = new StringBuilder("\n"),
          pending: Int = 0
      ): (Int, String) = instructions match {
        case Nil => (part1Acc, part2Acc.result())
        case head :: tail =>
          val lineEnd     = if (crtPosition == 39) "\n" else ""
          val pixel       = (if (Seq(x - 1, x, x + 1).contains(crtPosition)) "#" else " ") + lineEnd
          val newPart1Acc = if (interestingCycles.contains(cycle)) part1Acc + (x * cycle) else part1Acc
          val newPart2Acc = part2Acc.append(pixel)
          if (pending != 0) {
            helper(instructions, x + pending, cycle + 1, (crtPosition + 1) % 40, newPart1Acc, newPart2Acc)
          } else {
            head match {
              case addX(amount) =>
                helper(tail, x, cycle + 1, (crtPosition + 1) % 40, newPart1Acc, newPart2Acc, amount.toInt)
              case _ => helper(tail, x, cycle + 1, (crtPosition + 1) % 40, newPart1Acc, newPart2Acc)
            }
          }
      }

      answer = helper(input)
      solved = true
    }

    answer
  }
}

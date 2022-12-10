package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day10.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2:\n${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): List[String] = file.getLines().toList
  protected[year2022] def part1(input: List[String]): Int        = solution(input)._1
  protected[year2022] def part2(input: List[String]): String     = solution(input)._2

  // Both parts require the same traversal, so might as well only do it once
  private var part1Solution = 0
  private var part2Solution = ""
  private var solved        = false
  private def solution(input: List[String]): (Int, String) = {
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
        part2Acc: StringBuilder = new StringBuilder,
        pending: Int = 0
    ): Unit = instructions match {
      case Nil =>
        part1Solution = part1Acc
        part2Solution = part2Acc.mkString
        solved = true
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

    if (!solved) {
      helper(input)
    }
    (part1Solution, part2Solution)
  }
}

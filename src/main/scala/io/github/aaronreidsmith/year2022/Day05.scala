package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Solution, using}

import scala.collection.mutable
import scala.io.Source

object Day05 extends Solution(2022, 5) {
  type I = (Map[Int, String], List[String])
  type O1 = String
  type O2 = String

  override protected[year2022] def parseInput(file: Source): (Map[Int, String], List[String]) = {
    val Array(stackString, instructionString, _*) = file.mkString.split("\n\n")

    // Transpose our stacks and then parse them into map like this: Map(1 -> 'NZ', 2 -> 'DCM', 3 -> 'P')
    val stacks     = stackString.split('\n').toList
    val maxLength  = stacks.map(_.length).max // Have to make sure all lines are the same length for transpose
    val transposed = stacks.map(_.padTo(maxLength, ' ')).transpose.map(_.mkString)

    val numbersAndDigits = "[A-Z1-9]".r
    val parsedStacks = transposed.foldLeft(Map.empty[Int, String]) { (acc, stack) =>
      if (numbersAndDigits.findFirstIn(stack).isEmpty) { // Filter the transpositions that ended up as just spaces and brackets
        acc
      } else {
        val boxes  = stack.filter(_.isLetter)
        val number = stack.filter(_.isDigit).head.asDigit
        acc + (number -> boxes)
      }
    }

    (parsedStacks, instructionString.split('\n').toList)
  }

  override protected[year2022] def part1(input: (Map[Int, String], List[String])): String = solution(input, part2 = false)
  override protected[year2022] def part2(input: (Map[Int, String], List[String])): String = solution(input, part2 = true)

  private def solution(input: (Map[Int, String], List[String]), part2: Boolean): String = {
    val (originalStacks, instructions) = input

    // Turn our Map[Int, String] into Map[Int, mutable.Stack[Char]] so we have all the nice stack operations
    // Do this here instead of `parseInput` so we are not passing around mutable state (we'd have to reset between parts
    // 1 and 2 in that case)
    val stacks = originalStacks.view.mapValues(mutable.Stack.from(_)).toMap

    val instruction = """^move (\d+) from (\d) to (\d)$""".r
    val updated = instructions.foldLeft(stacks) {
      case (acc, instruction(numStr, fromStr, toStr)) =>
        val num  = numStr.toInt
        val from = fromStr.toInt
        val to   = toStr.toInt
        if (part2) {
          val popped = (0 until num).map(_ => acc(from).pop()).reverse
          acc(to).pushAll(popped)
        } else {
          (0 until num).foreach { _ =>
            val popped = acc(from).pop()
            acc(to).push(popped)
          }
        }
        acc
      case (acc, _) => acc
    }

    updated.toList
      .sortBy { case (num, _) => num }
      .map { case (_, stack) => stack.head }
      .mkString
  }
}

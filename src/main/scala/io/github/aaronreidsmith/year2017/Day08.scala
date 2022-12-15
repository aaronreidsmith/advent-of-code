package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day08 extends Solution(2017, 8) {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  override protected[year2017] def parseInput(file: Source): List[String] = file.getLines().toList
  override protected[year2017] def part1(input: List[String]): Int        = solution(input)._1
  override protected[year2017] def part2(input: List[String]): Int        = solution(input)._2

  // Both parts of the solution require the same traversal, so might as well only do it once
  private var part1Solution = 0
  private var part2Solution = 0
  private var solved        = false
  private def solution(instructions: List[String]): (Int, Int) = {
    if (!solved) {
      val instruction = """^([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (>|>=|<|<=|==|!=) (-?\d+)$""".r
      val processed = instructions.foldLeft(Map.empty[String, Int]) {
        case (acc, instruction(register, direction, amount, conditionRegister, operator, conditionValue)) =>
          val baseValue     = acc.getOrElse(register, 0)
          val registerValue = acc.getOrElse(conditionRegister, 0)
          val conditionVal  = conditionValue.toInt
          val conditionSatisfied = operator match {
            case "<"  => registerValue < conditionVal
            case "<=" => registerValue <= conditionVal
            case "==" => registerValue == conditionVal
            case ">=" => registerValue >= conditionVal
            case ">"  => registerValue > conditionVal
            case "!=" => registerValue != conditionVal
            case _    => throw new IllegalArgumentException
          }
          if (conditionSatisfied) {
            val delta = direction match {
              case "inc" => amount.toInt
              case "dec" => -amount.toInt
              case _     => throw new IllegalArgumentException
            }
            val newValue = baseValue + delta
            part2Solution = part2Solution.max(newValue)
            acc + (register -> newValue)
          } else {
            acc + (register -> baseValue)
          }
        case (acc, _) => acc
      }
      part1Solution = processed.values.max
      solved = true
    }

    (part1Solution, part2Solution)
  }
}

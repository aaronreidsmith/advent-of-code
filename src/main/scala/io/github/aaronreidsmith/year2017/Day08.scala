package io.github.aaronreidsmith.year2017

import scala.io.Source

object Day08 {
  private val instruction = "^(.*?) (inc|dec) (-?\\d+) if (.*?) (>|>=|<|<=|==|!=) (-?\\d+)$".r(
    "register",
    "direction",
    "amount",
    "conditionRegister",
    "operator",
    "conditionValue"
  )

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2017/day08.txt")

    var maxSeen: Option[Int] = None
    val instructions = input.getLines().foldLeft(Map.empty[String, Int]) {
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
          maxSeen match {
            case Some(value) => maxSeen = if (newValue > value) Some(newValue) else maxSeen
            case None        => maxSeen = Some(newValue)
          }
          acc + (register -> newValue)
        } else {
          acc + (register -> baseValue)
        }
    }
    input.close()
    println(s"Part 1: ${instructions.values.max}")
    println(s"Part 2: ${maxSeen.get}")
  }
}

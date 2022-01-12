package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.io.Source

// TODO: Don't really like all the `Either`s in here, but it works
object Day19 {
  private[year2020] sealed trait Rule
  private[year2020] case class Letter(value: Char)                 extends Rule
  private[year2020] case class SubRule(value: Vector[Vector[Int]]) extends Rule

  def main(args: Array[String]): Unit = {
    val (rules, messages) = using("2020/day19.txt")(parseInput)
    println(s"Part 1: ${part1(rules, messages)}")
    println(s"Part 2: ${part2(rules, messages)}")
  }

  private[year2020] def parseInput(file: Source): (Map[Int, Rule], List[String]) = {
    val Array(rawRules, rawMessages) = file.mkString.split("\n\n", 2)
    val rules = rawRules.split('\n').foldLeft(Map.empty[Int, Rule]) { (acc, line) =>
      val Array(ruleId, rawContents) = line.split(": ", 2)
      val contents                   = rawContents.replaceAll("\"", "")
      val rule = if (contents.matches("^[a-z]$")) {
        Letter(contents.head)
      } else {
        val subrules = contents.split(" \\| ").map(_.split(' ').toVector.map(_.toInt)).toVector
        SubRule(subrules)
      }
      acc.updated(ruleId.toInt, rule)
    }
    val messages = rawMessages.split('\n').toList
    (rules, messages)
  }

  private[year2020] def part1(rules: Map[Int, Rule], messages: List[String]): Int = {
    val stack = getInitialStack(rules)
    messages.count(message => isMatch(rules, message, stack))
  }

  private[year2020] def part2(rules: Map[Int, Rule], messages: List[String]): Int = {
    val updatedRules = rules ++ Map(
      8  -> SubRule(Vector(Vector(42), Vector(42, 8))),
      11 -> SubRule(Vector(Vector(42, 31), Vector(42, 11, 31)))
    )
    val stack = getInitialStack(updatedRules)
    messages.count(message => isMatch(updatedRules, message, stack))
  }

  private def getInitialStack(rules: Map[Int, Rule]): Vector[Either[Char, Int]] = rules(0) match {
    case Letter(value)     => Vector(Left(value))
    case SubRule(subrules) => subrules.head.map(Right(_))
  }

  private def isMatch(rules: Map[Int, Rule], message: String, stack: Vector[Either[Char, Int]]): Boolean = {
    if (stack.length > message.length) {
      false
    } else if (stack.isEmpty || message.isEmpty) {
      stack.isEmpty && message.isEmpty
    } else {
      val entry +: remaining = stack
      entry match {
        case Left(value) => value == message.head && isMatch(rules, message.tail, remaining)
        case Right(ruleId) =>
          rules(ruleId) match {
            case Letter(value)     => isMatch(rules, message, Left(value) +: remaining)
            case SubRule(subrules) => subrules.exists(rule => isMatch(rules, message, rule.map(Right(_)) ++ remaining))
          }
      }
    }
  }
}

package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

// TODO: Don't really like all the `Either`s in here, but it works
object Day19 extends Solution {
  type I  = (Map[Int, Rule], List[String])
  type O1 = Int
  type O2 = Int

  sealed trait Rule
  case class Letter(value: Char)                 extends Rule
  case class SubRule(value: Vector[Vector[Int]]) extends Rule

  override def parseInput(file: Source): (Map[Int, Rule], List[String]) = {
    val Array(rawRules, rawMessages) = file.mkString.trim.split("\n\n", 2)
    val rules = rawRules.split('\n').foldLeft(Map.empty[Int, Rule]) { (acc, line) =>
      val Array(ruleId, rawContents, _*) = line.split(": ", 2): @unchecked
      val contents                       = rawContents.replaceAll("\"", "")
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

  override def part1(input: (Map[Int, Rule], List[String])): Int = {
    val (rules, messages) = input
    val stack             = getInitialStack(rules)
    messages.count(message => isMatch(rules, message, stack))
  }

  override def part2(input: (Map[Int, Rule], List[String])): Int = {
    val (rules, messages) = input
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
      val entry +: remaining = stack: @unchecked
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

package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day19 extends Solution {
  type I  = (Map[Int, Rule], List[String])
  type O1 = Int
  type O2 = Int

  opaque type Character = Int | Char

  enum Rule {
    case Letter(val value: Char)
    case SubRule(val value: Vector[Vector[Int]])
  }

  override def parseInput(file: Source): (Map[Int, Rule], List[String]) = {
    val Array(rawRules, rawMessages) = file.mkString.trim.split("\n\n", 2)
    val rules = rawRules.split('\n').foldLeft(Map.empty[Int, Rule]) { (acc, line) =>
      val Array(ruleId, rawContents, _*) = line.split(": ", 2): @unchecked
      val contents                       = rawContents.replaceAll("\"", "")
      val rule = if (contents.matches("^[a-z]$")) {
        Rule.Letter(contents.head)
      } else {
        val subrules = contents.split(" \\| ").map(_.split(' ').toVector.map(_.toInt)).toVector
        Rule.SubRule(subrules)
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
      8  -> Rule.SubRule(Vector(Vector(42), Vector(42, 8))),
      11 -> Rule.SubRule(Vector(Vector(42, 31), Vector(42, 11, 31)))
    )
    val stack = getInitialStack(updatedRules)
    messages.count(message => isMatch(updatedRules, message, stack))
  }

  private def getInitialStack(rules: Map[Int, Rule]): Vector[Character] = rules(0) match {
    case Rule.Letter(value)     => Vector(value)
    case Rule.SubRule(subrules) => subrules.head
  }

  private def isMatch(rules: Map[Int, Rule], message: String, stack: Vector[Character]): Boolean = {
    if (stack.length > message.length) {
      false
    } else if (stack.isEmpty || message.isEmpty) {
      stack.isEmpty && message.isEmpty
    } else {
      val entry +: remaining = stack: @unchecked
      entry match {
        case value: Char => value == message.head && isMatch(rules, message.tail, remaining)
        case ruleId: Int =>
          rules(ruleId) match {
            case Rule.Letter(value)     => isMatch(rules, message, value +: remaining)
            case Rule.SubRule(subrules) => subrules.exists(rule => isMatch(rules, message, rule ++ remaining))
          }
      }
    }
  }
}

package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day07 extends Solution {
  type I  = Map[Bag, Rule]
  type O1 = Int
  type O2 = Int

  case class Bag(adjective: String, color: String) {
    def containsTarget(target: Bag, rules: Map[Bag, Rule]): Boolean = {
      val rule = rules(this)
      rule.contents.exists { pair =>
        val (bag, _) = pair
        bag == target || bag.containsTarget(target, rules)
      }
    }

    def getContents(rules: Map[Bag, Rule]): Int = rules(this).contents.foldLeft(0) {
      case (acc, (bag, quantity)) => acc + quantity + (quantity * bag.getContents(rules))
    }
  }

  case class Rule(bag: Bag, contents: Array[(Bag, Int)])

  private val target = Bag("shiny", "gold")

  override def parseInput(file: Source): Map[Bag, Rule] = {
    file.getLines().foldLeft(Map.empty[Bag, Rule]) { (acc, line) =>
      val Array(bagDesc, contentsString, _*) = line.split(" contain "): @unchecked
      val Array(adjective, color, _*)        = bagDesc.split(' '): @unchecked
      val bag                                = Bag(adjective, color)
      val contents = contentsString match {
        case "no other bags." => Array.empty[(Bag, Int)]
        case _ =>
          contentsString.split(", ").map { entry =>
            val Array(quantity, adjective, color, _*) = entry.split(' '): @unchecked
            (Bag(adjective, color), quantity.toInt)
          }
      }
      val rule = Rule(bag, contents)
      acc + (bag -> rule)
    }
  }

  override def part1(input: Map[Bag, Rule]): Int = input.keys.count(_.containsTarget(target, input))
  override def part2(input: Map[Bag, Rule]): Int = target.getContents(input)
}

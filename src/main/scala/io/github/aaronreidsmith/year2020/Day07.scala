package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

object Day07 {
  private case class Bag(adjective: String, color: String) {
    def containsTarget(target: Bag, rules: Map[Bag, Rule]): Boolean = {
      val rule = rules(this)
      rule.contents.exists {
        case (bag, _) => bag == target || bag.containsTarget(target, rules)
      }
    }

    def getContents(rules: Map[Bag, Rule]): Int = rules(this).contents.foldLeft(0) {
      case (acc, (bag, quantity)) => acc + quantity + (quantity * bag.getContents(rules))
    }
  }

  private case class Rule(bag: Bag, contents: List[(Bag, Int)])

  def main(args: Array[String]): Unit = {
    val rules  = using("2020/day07.txt")(_.getLines().toList.map(parseLine).toMap)
    val target = Bag("shiny", "gold")
    val part1 = rules.keys.foldLeft(0) {
      case (acc, bag) if bag.containsTarget(target, rules) => acc + 1
      case (acc, _)                                        => acc
    }
    println(s"Part 1: $part1")
    println(s"Part 2: ${target.getContents(rules)}")
  }

  private def parseLine(line: String): (Bag, Rule) = {
    val Array(bagDesc, contentsString) = line.split(" contain ")
    val Array(adjective, color, _*)        = bagDesc.split(' ')
    val bag                            = Bag(adjective, color)
    val contents = contentsString match {
      case "no other bags." => Nil
      case _ =>
        contentsString.split(", ").foldLeft(List.empty[(Bag, Int)]) { (acc, entry) =>
          val Array(quantity, adjective, color, _*) = entry.split(' ')
          (Bag(adjective, color), quantity.toInt) :: acc
        }
    }
    val rule = Rule(bag, contents)
    (bag, rule)
  }
}

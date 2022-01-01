package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Day19 {
  private[year2015] case class Rule(original: String, replacement: String) {
    val regex: Regex = original.r
  }

  def main(args: Array[String]): Unit = {
    val (rules, molecule) = using("2015/day19.txt")(parseInput)
    println(s"Part 1: ${part1(molecule, rules)}")
    println(s"Part 2: ${part2(molecule, rules)}")
  }

  private[year2015] def parseInput(file: Source): (List[Rule], String) = {
    val rule                       = "^(.*?) => (.*?)$".r
    val Array(ruleInput, molecule) = file.mkString.split("\n\n", 2)
    val rules = ruleInput.split('\n').foldLeft(List.empty[Rule]) {
      case (acc, rule(original, replacement)) => Rule(original, replacement) :: acc
      case (acc, _)                           => acc
    }
    (rules, molecule)
  }

  private[year2015] def part1(molecule: String, rules: List[Rule]): Int = rules
    .foldLeft(Set.empty[String]) { (acc, currentRule) =>
      val newMolecules = mutable.Set.empty[String]
      val matches      = currentRule.regex.findAllIn(molecule)
      while (matches.hasNext) {
        val newMolecule = molecule.take(matches.start) + currentRule.replacement + molecule.drop(matches.end)
        newMolecules += newMolecule
        matches.next()
      }
      acc ++ newMolecules
    }
    .size

  private[year2015] def part2(molecule: String, rules: List[Rule]): Int = {
    val replacements = rules.map(rule => rule.replacement.reverse -> rule.original.reverse).toMap
    val pattern      = replacements.keys.mkString("|")
    val regex        = pattern.r

    @tailrec
    def helper(reversedMolecule: String, count: Int): Int = if (reversedMolecule == "e") {
      count
    } else {
      val matches = regex.findAllIn(reversedMolecule)
      val updated = reversedMolecule.replaceFirst(pattern, replacements(matches.group(0)))
      helper(updated, count + 1)
    }

    helper(molecule.reverse, 0)
  }
}

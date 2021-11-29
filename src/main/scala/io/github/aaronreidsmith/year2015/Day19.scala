package io.github.aaronreidsmith.year2015

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Day19 {
  private val rule = "^(.*?) => (.*?)$".r

  protected[this] case class Rule(original: String, replacement: String) {
    val regex: Regex  = original.r
  }

  def main(args: Array[String]): Unit = {
    val input                          = Source.fromResource("2015/day19.txt")
    val Array(ruleInput, molecule, _*) = input.mkString.split("\n\n")
    val rules = ruleInput.split('\n').foldLeft(List.empty[Rule]) {
      case (acc, rule(original, replacement)) => acc :+ Rule(original, replacement)
      case _                                  => throw new IllegalArgumentException
    }
    val part1 = rules
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
    println(s"Part 1: $part1")
    // Part 2 is in python
  }
}

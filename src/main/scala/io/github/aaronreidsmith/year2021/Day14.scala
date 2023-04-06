package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day14 extends Solution {
  type I  = (String, Map[String, String])
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): (String, Map[String, String]) = {
    val Array(template, rawRules, _*) = file.mkString.trim.split("\n\n")
    val rules = rawRules.split('\n').foldLeft(Map.empty[String, String]) { (acc, line) =>
      val Array(key, value, _*) = line.split(" -> ")
      acc + (key -> value)
    }
    (template, rules)
  }

  override def part1(input: (String, Map[String, String])): Long = solution(input, 10)
  override def part2(input: (String, Map[String, String])): Long = solution(input, 40)

  private def solution(input: (String, Map[String, String]), iterations: Int): Long = {
    val (template, rules) = input
    val pairs             = mutable.Map(template.sliding(2).toSeq.map(_ -> 1L): _*)
    val chars             = mutable.Map(template.map(char => char -> template.count(_ == char).toLong): _*)
    (1 to iterations).foreach { _ =>
      val pairsCopy = Map.from(pairs)
      pairsCopy.foreach {
        case (pair, count) =>
          val first        = pair.head
          val second       = pair.tail
          val inserted     = rules(pair)
          val insertedChar = inserted.head

          val existingPairCount = pairs.getOrElse(pair, 1L)
          pairs.update(pair, existingPairCount - count)

          val firstNewPairKey   = s"$first$inserted"
          val firstNewPairCount = pairs.getOrElse(firstNewPairKey, 0L)
          pairs.update(firstNewPairKey, firstNewPairCount + count)

          val secondNewPairKey   = s"$inserted$second"
          val secondNewPairCount = pairs.getOrElse(secondNewPairKey, 0L)
          pairs.update(secondNewPairKey, secondNewPairCount + count)

          val insertedCharCount = chars.getOrElse(insertedChar, 0L)
          chars.update(insertedChar, insertedCharCount + count)
      }
    }
    chars.values.max - chars.values.min
  }
}

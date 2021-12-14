package io.github.aaronreidsmith.year2021

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day14 {
  def main(args: Array[String]): Unit = {
    val input                         = Using.resource(Source.fromResource("2021/day14.txt"))(_.mkString)
    val Array(template, rawRules, _*) = input.split("\n\n")
    val rules = rawRules
      .split('\n')
      .map { line =>
        val Array(key, value, _*) = line.split(" -> ")
        key -> value
      }
      .toMap

    println(s"Part 1: ${solution(template, rules, 10)}")
    println(s"Part 2: ${solution(template, rules, 40)}")
  }

  private def solution(template: String, rules: Map[String, String], iterations: Int): Long = {
    val pairs: mutable.Map[String, Long] = mutable.Map(template.sliding(2).toSeq.map(_ -> 1L): _*)
    val chars: mutable.Map[Char, Long]   = mutable.Map(template.map(char => char -> template.count(_ == char).toLong): _*)
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

          val firstNewPairKey   = first + inserted
          val firstNewPairCount = pairs.getOrElse(firstNewPairKey, 0L)
          pairs.update(firstNewPairKey, firstNewPairCount + count)

          val secondNewPairKey   = inserted + second
          val secondNewPairCount = pairs.getOrElse(secondNewPairKey, 0L)
          pairs.update(secondNewPairKey, secondNewPairCount + count)

          val insertedCharCount = chars.getOrElse(insertedChar, 0L)
          chars.update(insertedChar, insertedCharCount + count)
      }
    }
    chars.values.max - chars.values.min
  }
}

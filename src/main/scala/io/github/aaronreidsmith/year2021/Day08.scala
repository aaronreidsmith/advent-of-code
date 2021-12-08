package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day08 {
  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day08.txt"))(_.getLines().toSeq)

    val part1 = input.foldLeft(0) { (acc, line) =>
      val outputs = line.split(" \\| ").last.split(' ')
      acc + outputs.count(isEasyDigit)
    }
    println(s"Part 1: $part1")

    val part2 = {
      // (size, overlaps with `1`, overlaps with `4`) -> number
      val overlapMapping = Map(
        (6, 2, 3) -> 0,
        (2, 2, 2) -> 1,
        (5, 1, 2) -> 2,
        (5, 2, 3) -> 3,
        (4, 2, 4) -> 4,
        (5, 1, 3) -> 5,
        (6, 1, 3) -> 6,
        (3, 2, 2) -> 7,
        (7, 2, 4) -> 8,
        (6, 2, 4) -> 9
      )

      input.foldLeft(0) { (acc, line) =>
        val Array(inputs, outputs) = line.split(" \\| ").map(_.split(' '))
        val one                    = inputs.find(_.length == 2).getOrElse("").toSet
        val four                   = inputs.find(_.length == 4).getOrElse("").toSet
        val localMapping = inputs.map { string =>
          def numOverlaps(other: Set[Char]): Int = string.toSet.intersect(other).size
          val key                                = (string.length, numOverlaps(one), numOverlaps(four))
          string.sorted -> overlapMapping(key)
        }.toMap
        acc + outputs.map(output => localMapping(output.sorted)).mkString.toInt
      }
    }
    println(s"Part 2: $part2")
  }

  private def isEasyDigit(string: String): Boolean = string.length match {
    case 2 | 3 | 4 | 7 => true
    case _             => false
  }
}

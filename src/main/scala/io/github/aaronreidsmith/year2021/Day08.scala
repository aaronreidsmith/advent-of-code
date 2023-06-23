package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day08 extends Solution {
  type I  = Seq[(Seq[String], Seq[String])]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Seq[(Seq[String], Seq[String])] = file.getLines().toSeq.map { line =>
    val Array(inputs, outputs, _*) = line.split(" \\| ").map(_.split(' ').toSeq): @unchecked
    (inputs, outputs)
  }

  override def part1(input: Seq[(Seq[String], Seq[String])]): Int = {
    val easyDigits = Set(2, 3, 4, 7)
    input.foldLeft(0) {
      case (acc, (_, outputs)) => acc + outputs.count(output => easyDigits.contains(output.length))
    }
  }

  override def part2(input: Seq[(Seq[String], Seq[String])]): Int = {
    def numOverlaps(string: String, other: Set[Char]): Int = string.toSet.intersect(other).size

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

    input.foldLeft(0) {
      case (acc, (inputs, outputs)) =>
        val one  = inputs.find(_.length == 2).getOrElse("").toSet
        val four = inputs.find(_.length == 4).getOrElse("").toSet
        val localMapping = inputs.map { string =>
          val key = (string.length, numOverlaps(string, one), numOverlaps(string, four))
          string.sorted -> overlapMapping(key)
        }.toMap
        acc + outputs.map(output => localMapping(output.sorted)).mkString.toInt
    }
  }
}

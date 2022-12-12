package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.implicits._

import scala.io.Source

object Day02 extends Solution(2018, 2) {
  type I = List[String]
  type O1 = Int
  type O2 = String

  override protected[year2018] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2018] def part1(words: List[String]): Int = {
    val dupes = words.count(_.letterOccurrences.values.exists(_ == 2))
    val trips = words.count(_.letterOccurrences.values.exists(_ == 3))
    dupes * trips
  }

  override protected[year2018] def part2(words: List[String]): String = {
    def differingIndices(wordA: String, wordB: String): Vector[Int] =
      wordA.zip(wordB).zipWithIndex.foldLeft(Vector.empty[Int]) {
        case (acc, ((a, b), index)) if a != b => acc :+ index
        case (acc, _)                         => acc
      }

    words
      .combinations(2)
      .collectFirst {
        case List(a, b, _*) if differingIndices(a, b).size == 1 =>
          val index = differingIndices(a, b).head // TODO: Is there a way to not call this again?
          a.take(index) + a.drop(index + 1)
      }
      .getOrElse("")
  }
}

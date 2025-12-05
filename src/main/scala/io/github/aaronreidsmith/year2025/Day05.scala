package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day05 extends Solution {
  type I  = (Vector[(Long, Long)], Seq[Long])
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): (Vector[(Long, Long)], Seq[Long]) = {
    val Array(databaseRaw, ingredientsRaw, _*) = file.mkString.split("\n\n"): @unchecked
    val database = databaseRaw
      .split('\n')
      .toSeq
      .map { line =>
        val Array(left, right, _*) = line.split('-'): @unchecked
        (left.toLong, right.toLong)
      }
      // Combine ranges that overlap
      .sortBy(_._1)
      .foldLeft(Vector.empty[(Long, Long)]) {
        case (init :+ (lastStart, lastEnd), (start, end)) if start <= lastEnd =>
          init :+ (lastStart, math.max(lastEnd, end))
        case (acc, range) =>
          acc :+ range
      }
    val ingredients = ingredientsRaw.split('\n').toSeq.map(_.toLong)
    (database, ingredients)
  }

  override def part1(input: (Vector[(Long, Long)], Seq[Long])): Int = {
    val (database, ingredients) = input
    ingredients.count { id =>
      database.exists { (start, end) =>
        start <= id && id <= end
      }
    }
  }

  override def part2(input: (Vector[(Long, Long)], Seq[Long])): Long = {
    input._1.foldLeft(0L) { case (acc, (start, end)) => acc + (end - start + 1L) }
  }
}

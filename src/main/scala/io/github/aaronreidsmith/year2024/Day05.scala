package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day05 extends Solution {
  type I  = (Set[(Int, Int)], List[List[Int]])
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): (Set[(Int, Int)], List[List[Int]]) = {
    val Array(rulesRaw, updatesRaw, _*) = file.mkString.split("\n\n"): @unchecked
    val rules = rulesRaw
      .lines()
      .map { line =>
        val Array(left, right, _*) = line.split('|'): @unchecked
        (left.toInt, right.toInt)
      }
      .toList
      .asScala
      .toSet
    val updates = updatesRaw
      .lines()
      .map(line => line.split(',').map(_.toInt).toList)
      .toList
      .asScala
      .toList
    (rules, updates)
  }

  override def part1(input: (Set[(Int, Int)], List[List[Int]])): Int = solution(input, part2 = false)
  override def part2(input: (Set[(Int, Int)], List[List[Int]])): Int = solution(input, part2 = true)

  private def solution(input: (Set[(Int, Int)], List[List[Int]]), part2: Boolean): Int = {
    val (rules, updates) = input
    updates.foldLeft(0) { (acc, update) =>
      val sorted = update.sortWith((a, b) => rules.contains((a, b)))
      if ((!part2 && update == sorted) || (part2 && update != sorted)) {
        val middle = sorted(update.length / 2)
        acc + middle
      } else {
        acc
      }
    }
  }
}

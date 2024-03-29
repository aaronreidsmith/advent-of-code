package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{FlatCoordinate, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[String] = file.mkString.trim.split(',').toList
  override def part1(input: List[String]): Int        = solution(input)._1
  override def part2(input: List[String]): Int        = solution(input)._2

  // Both parts require the same traversal, so might as well only make it once
  private var part1Solution = 0
  private var part2Solution = Int.MinValue
  private var solved        = false
  private def solution(input: List[String]): (Int, Int) = {
    @tailrec
    def helper(directions: List[String], coordinate: FlatCoordinate): Unit = directions match {
      case Nil =>
        part1Solution = coordinate.distanceFrom(FlatCoordinate.zero)
        solved = true
      case head :: tail =>
        val newCoordinate   = coordinate.move(head)
        val currentDistance = newCoordinate.distanceFrom(FlatCoordinate.zero)
        part2Solution = part2Solution.max(currentDistance)
        helper(tail, newCoordinate)
    }

    if (!solved || isTest) {
      helper(input, FlatCoordinate.zero)
    }

    (part1Solution, part2Solution)
  }
}

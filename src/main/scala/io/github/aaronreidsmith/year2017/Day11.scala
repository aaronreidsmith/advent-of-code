package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{FlatCoordinate, Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends Solution(2017, 11) {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  override protected[year2017] def parseInput(file: Source): List[String] = file.mkString.split(',').toList
  override protected[year2017] def part1(input: List[String]): Int        = solution(input)._1
  override protected[year2017] def part2(input: List[String]): Int        = solution(input)._2

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
        val newCoordinate = head match {
          case "n"  => coordinate.north
          case "ne" => coordinate.northEast
          case "se" => coordinate.southEast
          case "s"  => coordinate.south
          case "sw" => coordinate.southWest
          case "nw" => coordinate.northWest
          case _    => throw new IllegalArgumentException
        }
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

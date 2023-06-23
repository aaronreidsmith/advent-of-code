package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day12 extends Solution {
  type I  = Map[String, Set[String]]
  type O1 = Int
  type O2 = Int

  private val start = "start"
  private val end   = "end"

  override def parseInput(file: Source): Map[String, Set[String]] = {
    file
      .getLines()
      .toList
      .map(_.split('-'))
      .flatMap {
        case Array(start, end) => Set((start, end), (end, start))
        case _                 => Set()
      }
      .groupBy((start, _) => start)
      .map((start, neighbors) => start -> neighbors.map((_, end) => end).toSet)
  }

  override def part1(input: Map[String, Set[String]]): Int = solution(input, true)
  override def part2(input: Map[String, Set[String]]): Int = solution(input, false)

  private def solution(caves: Map[String, Set[String]], isSmallCaveVisitedTwice: Boolean): Int = {
    countPaths(start, caves, Set(start), isSmallCaveVisitedTwice)
  }

  private def countPaths(
      cave: String,
      caves: Map[String, Set[String]],
      visited: Set[String],
      isSmallCaveVisitedTwice: Boolean
  ): Int = {
    val neighbors = if (isSmallCaveVisitedTwice) caves(cave) -- visited else caves(cave) - start
    if (cave == end) {
      1
    } else if (neighbors.isEmpty) {
      0
    } else {
      val newVisited = if (isSmall(cave)) visited + cave else visited
      neighbors.foldLeft(0) { (acc, neighbor) =>
        val newIsSmallCaveVisitedTwice = isSmallCaveVisitedTwice || (isSmall(neighbor) && visited.contains(neighbor))
        acc + countPaths(neighbor, caves, newVisited, newIsSmallCaveVisitedTwice)
      }
    }
  }

  private def isSmall(cave: String): Boolean = cave.head.isLower
}

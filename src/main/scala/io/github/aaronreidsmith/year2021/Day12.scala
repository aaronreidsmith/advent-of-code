package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day12.txt")) { file =>
      val segments = file.getLines().toList.map(_.split('-')).flatMap {
        case Array(start, end) => Set((start, end), (end, start))
      }
      segments
        .groupBy { case (start, _) => start }
        .map {
          case (start, neighbors) => start -> neighbors.map { case (_, end) => end }.toSet
        }
    }
    val start          = "start"
    val initialVisited = Set(start)
    println(s"Part 1: ${countPaths(start, input, initialVisited, isSmallCaveVisitedTwice = true)}")
    println(s"Part 2: ${countPaths(start, input, initialVisited, isSmallCaveVisitedTwice = false)}")
  }

  private def isSmall(cave: String): Boolean = cave.head.isLower

  private def countPaths(
      cave: String,
      caves: Map[String, Set[String]],
      visited: Set[String],
      isSmallCaveVisitedTwice: Boolean
  ): Int = {
    val neighbors = if (isSmallCaveVisitedTwice) caves(cave) -- visited else caves(cave) - "start"
    if (cave == "end") {
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
}

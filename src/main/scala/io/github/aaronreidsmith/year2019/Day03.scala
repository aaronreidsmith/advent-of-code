package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source

// TODO: Adapted from Raku, so just kind of ugly
object Day03 extends Solution(2019, 3) {
  type I  = (List[String], List[String])
  type O1 = Int
  type O2 = Int

  override protected[year2019] def parseInput(file: Source): (List[String], List[String]) = {
    val wires                   = file.mkString.trim.split('\n').map(_.split(',').toList)
    val Array(wire1, wire2, _*) = wires
    (wire1, wire2)
  }

  override protected[year2019] def part1(wires: (List[String], List[String])): Int = {
    getOverlaps(wires).foldLeft(Int.MaxValue) {
      case (currentMin, (a, b)) =>
        val distance = a.abs + b.abs
        if (distance > 0) currentMin.min(distance) else currentMin
      case (currentMin, _) => currentMin
    }
  }

  override protected[year2019] def part2(wires: (List[String], List[String])): Int = {
    val (wire1, wire2) = wires
    val wire1Path      = traverse(wire1)
    val wire2Path      = traverse(wire2)
    val overlaps       = getOverlaps(wires)
    val wire1Overlaps  = wire1Path.collect { case entry if overlaps.contains(entry._1) => entry }.toList.sorted
    val wire2Overlaps  = wire2Path.collect { case entry if overlaps.contains(entry._1) => entry }.toList.sorted
    wire1Overlaps.zip(wire2Overlaps).foldLeft(Int.MaxValue) {
      case (currentMin, pair) =>
        val distance = pair._1._2 + pair._2._2
        if (distance != 0) currentMin.min(distance) else currentMin
    }
  }

  private def getOverlaps(wires: (List[String], List[String])): Set[(Int, Int)] = {
    val (wire1, wire2) = wires
    val wire1Path      = traverse(wire1)
    val wire2Path      = traverse(wire2)
    wire1Path.map(_._1).intersect(wire2Path.map(_._1))
  }

  private def traverse(initialDirections: List[String]): Set[((Int, Int), Int)] = {
    // Only want to compile these once...
    val up    = "^U(\\d+)$".r
    val down  = "^D(\\d+)$".r
    val left  = "^L(\\d+)$".r
    val right = "^R(\\d+)$".r

    def helper(directions: List[String], position: (Int, Int) = (0, 0), pathLength: Int = 0): Set[((Int, Int), Int)] = {
      val (x, y) = position
      val (newPosition, spacesPassed, pathRange) = directions.head match {
        case up(value) =>
          val moves = value.toInt
          val newY  = y + moves
          ((x, newY), (y to newY).map((x, _)), pathLength to (pathLength + moves))
        case down(value) =>
          val moves = value.toInt
          val newY  = y - moves
          ((x, newY), (y to newY by -1).map((x, _)), pathLength to (pathLength + moves))
        case right(value) =>
          val moves = value.toInt
          val newX  = x + moves
          ((newX, y), (x to newX).map((_, y)), pathLength to (pathLength + moves))
        case left(value) =>
          val moves = value.toInt
          val newX  = x - moves
          ((newX, y), (x to newX by -1).map((_, y)), pathLength to (pathLength + moves))
        case _ => throw new IllegalArgumentException
      }
      if (directions.size > 1) {
        val current = spacesPassed.zip(pathRange).toSet
        current.union(helper(directions.tail, newPosition, pathRange.max))
      } else {
        spacesPassed.zip(pathRange).toSet
      }
    }

    helper(initialDirections)
  }
}

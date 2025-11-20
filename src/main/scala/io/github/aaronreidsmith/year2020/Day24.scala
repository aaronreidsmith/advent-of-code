package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{PointyCoordinate, Solution}

import scala.collection.mutable
import scala.io.Source

object Day24 extends Solution {
  type I  = List[List[String]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[List[String]] = {
    val directions = "e|se|sw|w|nw|ne".r
    file.getLines().toList.map(line => directions.findAllIn(line).toList)
  }

  override def part1(instructions: List[List[String]]): Int = initialFlip(instructions).size
  override def part2(instructions: List[List[String]]): Int = {
    val initial = initialFlip(instructions)
    (0 until 100)
      .foldLeft(initial) { (flipped, _) =>
        val (qs, rs)           = flipped.unzip
        val qMin               = qs.min
        val qMax               = qs.max
        val rMin               = rs.min
        val rMax               = rs.max
        val flippedThisRound   = mutable.Set.empty[PointyCoordinate]
        val unflippedThisRound = mutable.Set.empty[PointyCoordinate]
        for {
          q <- qMin - 1 to qMax + 1
          r <- rMin - 1 to rMax + 1
          tile = PointyCoordinate(q, r)
        } {
          val flippedNeighbors = tile.neighbors.count(flipped.contains)
          if (flipped.contains(tile)) {
            if (flippedNeighbors == 0 || flippedNeighbors > 2) {
              unflippedThisRound.add(tile)
            }
          } else {
            if (flippedNeighbors == 2) {
              flippedThisRound.add(tile)
            }
          }
        }
        (flipped ++ flippedThisRound) -- unflippedThisRound
      }
      .size
  }

  private def initialFlip(instructions: List[List[String]]): Set[PointyCoordinate] = {
    instructions.foldLeft(Set.empty[PointyCoordinate]) { (flipped, instructionSet) =>
      val tile = instructionSet.foldLeft(PointyCoordinate.zero)(_.move(_))
      if (flipped.contains(tile)) flipped - tile else flipped + tile
    }
  }
}

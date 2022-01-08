package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.collection.mutable
import scala.io.Source

object Day24 {
  implicit class HexCoordinateOps(point: (Int, Int)) {
    private val (q, r) = point
    def neighbors: Seq[(Int, Int)] = Seq(
      (q + 1, r),
      (q, r + 1),
      (q - 1, r + 1),
      (q - 1, r),
      (q, r - 1),
      (q + 1, r - 1)
    )
  }

  def main(args: Array[String]): Unit = {
    val input = using("2020/day24.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2020] def parseInput(file: Source): List[List[String]] = {
    val directions = "e|se|sw|w|nw|ne".r
    file.getLines().toList.map(line => directions.findAllIn(line).toList)
  }

  private[year2020] def part1(instructions: List[List[String]]): Int = initialFlip(instructions).size
  private[year2020] def part2(instructions: List[List[String]]): Int = {
    val initial = initialFlip(instructions)
    (0 until 100)
      .foldLeft(initial) { (flipped, _) =>
        val (qs, rs)           = flipped.unzip
        val qMin               = qs.min
        val qMax               = qs.max
        val rMin               = rs.min
        val rMax               = rs.max
        val flippedThisRound   = mutable.Set.empty[(Int, Int)]
        val unflippedThisRound = mutable.Set.empty[(Int, Int)]
        for {
          q <- qMin - 1 to qMax + 1
          r <- rMin - 1 to rMax + 1
          tile = (q, r)
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

  private def initialFlip(instructions: List[List[String]]): Set[(Int, Int)] = instructions
    .foldLeft(Set.empty[(Int, Int)]) { (flipped, instructionSet) =>
      val tile = instructionSet.foldLeft((0, 0)) {
        case ((q, r), direction) =>
          direction match {
            case "e"  => (q + 1, r)
            case "se" => (q, r + 1)
            case "sw" => (q - 1, r + 1)
            case "w"  => (q - 1, r)
            case "nw" => (q, r - 1)
            case "ne" => (q + 1, r - 1)
            case _    => throw new IllegalArgumentException
          }
      }
      if (flipped.contains(tile)) {
        flipped - tile
      } else {
        flipped + tile
      }
    }
}

package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Direction, Point, Solution}

import scala.io.Source

object Day12 extends Solution {
  type I  = Set[Region]
  type O1 = Int
  type O2 = Int

  type Region = Set[Point]

  extension (r: Region) {
    private def edges: Set[(Point, Direction)] = {
      r.foldLeft(Set.empty[(Point, Direction)]) { (size, point) =>
        size ++ Direction.values.collect {
          case direction if !r.contains(point.move(direction)) => (point, direction)
        }
      }
    }

    def area: Int      = r.size
    def perimeter: Int = edges.size
    def sides: Int     = (edges -- edges.map((point, direction) => (point.move(direction.right), direction))).size
  }

  // TODO: Could probably be made immutable, but this works
  override def parseInput(file: Source): Set[Region] = {
    val grid           = file.toGrid
    val regionsBuilder = grid.keySet.map(pos => pos -> Set(pos)).toMap.toMutable
    for {
      (pos, char) <- grid
      neighbor    <- pos.immediateNeighbors
    } {
      if (grid.get(neighbor).fold(false)(_ == char)) {
        regionsBuilder(pos) ++= regionsBuilder(neighbor)
        regionsBuilder(pos).foreach { x =>
          regionsBuilder(x) = regionsBuilder(pos)
        }
      }
    }
    regionsBuilder.values.toSet
  }

  override def part1(input: Set[Region]): Int = {
    input.foldLeft(0)((acc, region) => acc + (region.area * region.perimeter))
  }

  override def part2(input: Set[Region]): Int = {
    input.foldLeft(0)((acc, region) => acc + (region.area * region.sides))
  }
}

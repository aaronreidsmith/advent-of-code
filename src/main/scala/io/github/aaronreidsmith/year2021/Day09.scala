package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends Solution {
  type I  = Grid[Int]
  type O1 = Int
  type O2 = Int

  extension (grid: Grid[Int]) {
    def neighborsAreHigher(position: Point, height: Int): Boolean = {
      position.immediateNeighbors.forall(pos => grid.get(pos).forall(_ > height))
    }
  }

  override def parseInput(file: Source): Grid[Int] = file.toGrid.view.mapValues(_.asDigit).toMap

  override def part1(input: Grid[Int]): Int = input.foldLeft(0) {
    case (acc, (position, height)) if input.neighborsAreHigher(position, height) => acc + height + 1
    case (acc, _)                                                                => acc
  }

  override def part2(input: Grid[Int]): Int = {
    @tailrec
    def findBasin(currentBasin: Seq[Point]): Seq[Point] = {
      val updatedBasin = currentBasin.foldLeft(currentBasin) { (acc, position) =>
        val unseenNeighbors = position.immediateNeighbors.collect {
          case neighbor if input.get(neighbor).exists(_ < 9) && !acc.contains(neighbor) => neighbor
        }
        acc ++ unseenNeighbors
      }

      if (updatedBasin.size == currentBasin.size) currentBasin else findBasin(updatedBasin)
    }

    input
      .collect {
        case (position, height) if input.neighborsAreHigher(position, height) => findBasin(Seq(position)).size
      }
      .toSeq
      .sorted
      .takeRight(3)
      .product
  }
}

package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09 {
  private implicit class Tuple2IntOps(tuple: (Int, Int)) {
    def neighbors: Seq[(Int, Int)] = {
      val (row, col) = tuple
      Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day09.txt"))(_.getLines().toList)
    val grid = {
      for {
        (line, row)   <- input.zipWithIndex
        (height, col) <- line.zipWithIndex
      } yield (row, col) -> height.asDigit
    }.toMap

    def neighborsAreHigher(position: (Int, Int), height: Int): Boolean = {
      position.neighbors.forall(pos => grid.get(pos).forall(_ > height))
    }

    val part1 = grid.foldLeft(0) {
      case (acc, (position, height)) if neighborsAreHigher(position, height) => acc + height + 1
      case (acc, _)                                                          => acc
    }
    println(s"Part 1: $part1")

    @tailrec
    def findBasin(currentBasin: Seq[(Int, Int)], checkedPositions: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      val (updatedBasin, updatedCheckedPositions) = currentBasin.foldLeft((currentBasin, checkedPositions)) {
        case ((currentBasinAcc, checkedPositionsAcc), position) =>
          val unseenNeighbors = position.neighbors.collect {
            case neighbor if grid.get(neighbor).exists(_ < 9) && !checkedPositionsAcc.contains(neighbor) => neighbor
          }
          (currentBasinAcc ++ unseenNeighbors, checkedPositionsAcc ++ unseenNeighbors)
      }

      if (updatedBasin.size == currentBasin.size) currentBasin else findBasin(updatedBasin, updatedCheckedPositions)
    }

    val part2 = grid
      .collect {
        case (position, height) if neighborsAreHigher(position, height) => findBasin(Seq(position), Seq(position)).size
      }
      .toSeq
      .sorted
      .takeRight(3)
      .product
    println(s"Part 2: $part2")
  }
}

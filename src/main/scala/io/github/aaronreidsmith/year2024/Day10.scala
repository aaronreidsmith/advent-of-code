package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day10 extends Solution {
  type I  = Grid[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Int] = {
    file.toGrid.view.mapValues(_.asDigit).toMap
  }

  override def part1(input: Grid[Int]): Int = {
    @tailrec
    def score(toVisit: Queue[Point], peaks: Set[Point] = Set.empty): Int = {
      toVisit.headOption match {
        case None => peaks.size
        case Some(pos) =>
          input.get(pos) match {
            case Some(9) => score(toVisit.tail, peaks + pos)
            case Some(incline) =>
              val next = pos.immediateNeighbors.filter { p =>
                !toVisit.contains(p) && input.get(p).fold(false)(_ - incline == 1)
              }
              score(toVisit.tail ++ next, peaks)
            case None => score(toVisit.tail, peaks)
          }
      }
    }

    input.foldLeft(0) {
      case (acc, (pos, 0)) => acc + score(Queue(pos))
      case (acc, _)        => acc
    }
  }

  override def part2(input: Grid[Int]): Int = {
    def rating(position: Point, visited: Set[Point] = Set.empty): Int = {
      input.get(position) match {
        case Some(9) => 1
        case Some(incline) =>
          val next = position.immediateNeighbors.filter { p =>
            !visited.contains(p) && input.get(p).fold(false)(_ - incline == 1)
          }
          next.foldLeft(0)((acc, pos) => acc + rating(pos, visited + position))
        case None => 0
      }
    }

    input.foldLeft(0) {
      case (acc, (pos, 0)) => acc + rating(pos)
      case (acc, _)        => acc
    }
  }
}

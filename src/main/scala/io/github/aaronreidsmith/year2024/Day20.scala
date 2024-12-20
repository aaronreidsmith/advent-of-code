package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

@Slow(part1 = true)
object Day20 extends Solution {
  type I  = Map[Point, Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Map[Point, Int] = {
    val grid  = file.toGrid.filterNot((_, c) => c == '#')
    val start = grid.collectFirst { case (p, c) if c == 'S' => p }.get

    @tailrec
    def helper(todo: List[Point], dist: Map[Point, Int]): Map[Point, Int] = {
      todo match {
        case Nil => dist
        case head :: tail =>
          val newDistances = head.immediateNeighbors.collect {
            case p if grid.contains(p) && !dist.contains(p) => p -> (dist(head) + 1)
          }.toMap
          helper(tail ++ newDistances.keys, dist ++ newDistances)
      }
    }

    helper(List(start), Map(start -> 0))
  }

  override def part1(input: Map[Point, Int]): Int = solution(input)._1
  override def part2(input: Map[Point, Int]): Int = solution(input)._2

  private var part1Solution = 0
  private var part2Solution = 0
  private var solved        = false
  private def solution(distances: Map[Point, Int]): (Int, Int) = {
    if (!solved) {
      distances.toSeq.combinations(2).foreach {
        case Seq((p, i), (q, j)) =>
          val d = p.manhattanDistance(q)
          if (d == 2 && (j - i).abs - d >= (if (isTest) 2 else 100)) {
            part1Solution += 1
          }
          if (d < 21 && (j - i).abs - d >= (if (isTest) 50 else 100)) {
            part2Solution += 1
          }
      }
      solved = true
    }
    (part1Solution, part2Solution)
  }
}

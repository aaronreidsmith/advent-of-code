package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.extensions.mod

import scala.collection.mutable
import scala.io.Source

// Adapted from https://old.reddit.com/r/adventofcode/comments/zu28ij/2022_day_24_solutions/j1gs2ap/
object Day24 extends Solution {
  type I  = (Int, Int, Set[Blizzard], Set[Wall])
  type O1 = Int
  type O2 = Int

  case class Blizzard(x: Int, y: Int, dx: Int, dy: Int)
  opaque type Wall = (Int, Int) // Would prefer a case class, but this works nicer elsewhere

  override def parseInput(file: Source): (Int, Int, Set[Blizzard], Set[Wall]) = {
    val blizzards = mutable.Set.empty[Blizzard]
    val walls     = mutable.Set.empty[Wall]
    for {
      (line, y) <- file.getLines().zipWithIndex
      (char, x) <- line.zipWithIndex
    } {
      char match {
        case '^' => blizzards.add(Blizzard(x - 1, y - 1, 0, -1))
        case '>' => blizzards.add(Blizzard(x - 1, y - 1, 1, 0))
        case 'v' => blizzards.add(Blizzard(x - 1, y - 1, 0, 1))
        case '<' => blizzards.add(Blizzard(x - 1, y - 1, -1, 0))
        case '#' => walls.add((x - 1, y - 1))
        case _   => // do nothing
      }
    }

    val (xs, ys) = walls.unzip
    val maxX     = xs.max
    val maxY     = ys.max

    // Add some walls on the top and bottom, otherwise the player escapes the maze
    (-1 to 2).foreach(x => walls.add((x, -2)))
    (maxX - 3 to maxX + 1).foreach(x => walls.add((x, maxY + 1)))

    (maxX, maxY, blizzards.toSet, walls.toSet)
  }

  override def part1(input: (Int, Int, Set[Blizzard], Set[Wall])): Int = {
    val (maxX, maxY, blizzards, walls) = input
    solution(maxX, maxY, blizzards, walls)._1
  }

  override def part2(input: (Int, Int, Set[Blizzard], Set[Wall])): Int = {
    val (maxX, maxY, blizzards, walls) = input
    solution(maxX, maxY, blizzards, walls)._2
  }

  private var part1Answer = 0
  private var part2Answer = 0
  private var solved      = false
  private def solution(maxX: Int, maxY: Int, blizzards: Set[Blizzard], walls: Set[Wall]): (Int, Int) = {
    if (!solved) {
      val start = (0, -1)
      val exit  = (maxX - 1, maxY)
      val q     = mutable.Set(start)
      val goals = mutable.Queue(exit, start, exit)
      var t     = 0
      while (goals.nonEmpty) {
        t += 1
        val b = blizzards.map { blizzard =>
          ((blizzard.x + t * blizzard.dx) mod maxX, (blizzard.y + t * blizzard.dy) mod maxY)
        }
        val n = {
          for {
            (dx, dy) <- Seq((1, 0), (0, 1), (-1, 0), (0, -1), (0, 0))
            (px, py) <- q
          } yield (px + dx, py + dy)
        }.toSet
        q.clear()
        q.addAll(n -- b -- walls)
        if (q.contains(goals.head)) {
          if (goals.size == 3) {
            part1Answer = t
          }
          if (goals.size == 1) {
            part2Answer = t
          }
          q.clear()
          q.add(goals.removeHead())
        }
      }
      solved = true
    }

    (part1Answer, part2Answer)
  }
}

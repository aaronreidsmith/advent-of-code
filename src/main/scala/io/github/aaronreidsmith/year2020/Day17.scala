package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

// TODO: Adapted from Python solution, so a fair bit of mutability
object Day17 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  extension (list: List[Int]) {
    // Used to mimic Python's itertools.product
    def combinationsWithRepeats(n: Int): List[List[Int]] = n match {
      case 0 => List(Nil)
      case _ =>
        for {
          el <- list
          sl <- list.combinationsWithRepeats(n - 1)
        } yield el :: sl
    }

    def neighbors: List[List[Int]] = List(-1, 0, 1).combinationsWithRepeats(list.length).map { diff =>
      list.zipWithIndex.map {
        case (coordinate, i) => coordinate + diff(i)
      }
    }
  }

  override def parseInput(file: Source): List[String] = file.getLines().toList
  override def part1(initialState: List[String]): Int = solution(initialState, 3)
  override def part2(initialState: List[String]): Int = solution(initialState, 4)

  private def solution(initial: List[String], dimensions: Int): Int = {
    val space = mutable.Map.empty[List[Int], Char].withDefaultValue('.')
    for {
      (line, x)  <- initial.zipWithIndex
      (state, y) <- line.zipWithIndex
    } {
      val cube = List(x, y).padTo(dimensions, 0)
      space.update(cube, state)
    }

    (1 to 6).foreach { _ =>
      val active = mutable.Map.empty[List[Int], Int].withDefaultValue(0)

      for {
        (cube, state) <- space if state != '.'
        neighbor      <- cube.neighbors
      } {
        val existing = active(neighbor)
        val updated  = if (neighbor != cube) existing + 1 else existing
        active.update(neighbor, updated)
      }

      active.foreach {
        case (cube, activeNeighbors) =>
          if (space(cube) == '#' && !Seq(2, 3).contains(activeNeighbors)) {
            space.update(cube, '.')
          } else if (space(cube) == '.' && activeNeighbors == 3) {
            space.update(cube, '#')
          }
      }
    }

    space.values.count(_ == '#')
  }
}

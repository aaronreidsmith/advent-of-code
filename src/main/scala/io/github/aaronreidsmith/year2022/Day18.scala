package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.collection.mutable
import scala.io.Source

@Slow(part2 = true)
object Day18 extends Solution {
  type I  = Set[Cube]
  type O1 = Int
  type O2 = Int

  case class Cube(x: Int, y: Int, z: Int) {
    def neighbors: Set[Cube] = Set(
      this.copy(x = x - 1),
      this.copy(x = x + 1),
      this.copy(y = y - 1),
      this.copy(y = y + 1),
      this.copy(z = z - 1),
      this.copy(z = z + 1)
    )
  }

  given Conversion[Cube, (Int, Int, Int)] with {
    def apply(cube: Cube): (Int, Int, Int) = (cube.x, cube.y, cube.z)
  }

  override def parseInput(file: Source): Set[Cube] = {
    file.getLines().foldLeft(Set.empty[Cube]) { (acc, line) =>
      val Array(x, y, z, _*) = line.split(','): @unchecked
      acc + Cube(x.toInt, y.toInt, z.toInt)
    }
  }

  override def part1(input: Set[Cube]): Int = {
    input.foldLeft(0)((acc, cube) => acc + cube.neighbors.count(!input.contains(_)))
  }

  // Adapted from https://old.reddit.com/r/adventofcode/comments/zoqhvy/2022_day_18_solutions/j0oul0u/
  override def part2(input: Set[Cube]): Int = {
    val (xs, ys, zs) = input.unzip3
    // Have to pad our ranges for movement
    val xRange = -1 to xs.max + 1
    val yRange = -1 to ys.max + 1
    val zRange = -1 to zs.max + 1

    val seen = mutable.Set.empty[Cube]
    val todo = mutable.Stack(Cube(-1, -1, -1))
    while (todo.nonEmpty) {
      val here = todo.pop()
      val toVisit = here.neighbors.diff(input).diff(seen).filter { cube =>
        xRange.contains(cube.x) && yRange.contains(cube.y) && zRange.contains(cube.z)
      }
      todo.pushAll(toVisit)
      seen.add(here)
    }

    input.foldLeft(0)((acc, cube) => acc + cube.neighbors.count(seen.contains))
  }
}

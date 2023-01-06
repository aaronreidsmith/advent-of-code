package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends Solution(2019, 24) {
  type I  = Set[Point3D]
  type O1 = Int
  type O2 = Int

  private[year2019] case class Point3D(row: Int, col: Int, depth: Int) {
    def neighbors: Seq[Point3D] = Seq(
      copy(row = row - 1),
      copy(row = row + 1),
      copy(col = col - 1),
      copy(col = col + 1)
    )
  }

  override protected[year2019] def parseInput(file: Source): Set[Point3D] = {
    for {
      (line, x) <- file.getLines().zipWithIndex
      (char, y) <- line.zipWithIndex
      if char == '#'
    } yield Point3D(x, y, 0)
  }.toSet

  override protected[year2019] def part1(input: Set[Point3D]): Int = {
    @tailrec
    def helper(grid: Set[Point3D], previous: Set[Set[Point3D]] = Set.empty): Int = {
      val next = step(grid, neighbors2D)
      if (previous.contains(next)) {
        next.foldLeft(0) { (acc, point) =>
          val n = point.col + 5 * point.row
          acc + math.pow(2, n).toInt
        }
      } else {
        helper(next, previous + next)
      }
    }

    helper(input)
  }

  override protected[year2019] def part2(input: Set[Point3D]): Int = {
    val minutes = if (isTest) 10 else 200
    Iterator.iterate(input)(step(_, neighbors3D)).drop(minutes).next().size
  }

  private def neighbors2D(point: Point3D): Seq[Point3D] = point.neighbors.filter { neighbor =>
    0 <= neighbor.row && neighbor.row < 5 &&
    0 <= neighbor.col && neighbor.col < 5
  }

  private def neighbors3D(point: Point3D): Seq[Point3D] = point.neighbors.flatMap {
    case Point3D(-1, _, z) => Seq(Point3D(2, 1, z - 1))
    case Point3D(5, _, z)  => Seq(Point3D(2, 3, z - 1))
    case Point3D(_, -1, z) => Seq(Point3D(1, 2, z - 1))
    case Point3D(_, 5, z)  => Seq(Point3D(3, 2, z - 1))
    case Point3D(2, 2, _) =>
      point match {
        case Point3D(1, 2, z) => (0 until 5).map(Point3D(_, 0, z + 1))
        case Point3D(3, 2, z) => (0 until 5).map(Point3D(_, 4, z + 1))
        case Point3D(2, 1, z) => (0 until 5).map(Point3D(0, _, z + 1))
        case Point3D(2, 3, z) => (0 until 5).map(Point3D(4, _, z + 1))
        case _                => Seq()
      }
    case other => Seq(other)
  }

  private def step(grid: Set[Point3D], neighbors: Point3D => Seq[Point3D]): Set[Point3D] = {
    val candidates = grid ++ grid.flatMap(neighbors)
    candidates.filter { point =>
      neighbors(point).count(grid.contains) match {
        case 1 => true
        case 2 => !grid.contains(point)
        case _ => false
      }
    }
  }
}

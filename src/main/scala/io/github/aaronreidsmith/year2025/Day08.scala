package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.extensions.*

import scala.io.Source

object Day08 extends Solution {
  type I  = (Seq[Point3D], Seq[(Int, Int)])
  type O1 = Long
  type O2 = Long

  case class Point3D(x: Long, y: Long, z: Long) {
    def distance(other: Point3D): Long = {
      val dx = other.x - this.x
      val dy = other.y - this.y
      val dz = other.z - this.z
      (dx * dx) + (dy * dy) + (dz * dz)
    }
  }

  override def parseInput(file: Source): (Seq[Point3D], Seq[(Int, Int)]) = {
    val junctionBoxes = file.getLines().toSeq.map { line =>
      val Array(x, y, z, _*) = line.split(','): @unchecked
      Point3D(x.toLong, y.toLong, z.toLong)
    }
    val distances = for {
      (p1, i) <- junctionBoxes.iterator.zipWithIndex
      (p2, j) <- junctionBoxes.iterator.zipWithIndex
      if i > j
      distance = p1.distance(p2)
    } yield (distance, i, j)

    (junctionBoxes, distances.toSeq.sorted.map((_, i, j) => (i, j)))
  }

  override def part1(input: (Seq[Point3D], Seq[(Int, Int)])): Long = solution(input)._1
  override def part2(input: (Seq[Point3D], Seq[(Int, Int)])): Long = solution(input)._2

  private var solved        = false
  private var part1Solution = 0L
  private var part2Solution = 0L
  private def solution(input: (Seq[Point3D], Seq[(Int, Int)])): (Long, Long) = {
    if (solved) {
      (part1Solution, part2Solution)
    } else {
      val (junctionBoxes, distances) = input
      val state                      = junctionBoxes.indices.map(i => i -> i).toMap.toMutable
      val iterations                 = if (isTest) 10 else 1000

      def find(x: Int): Int = {
        if (x == state(x)) {
          x
        } else {
          state.update(x, find(state(x)))
          state(x)
        }
      }

      def union(x: Int, y: Int): Unit = {
        state.update(find(x), find(y))
      }

      var connections = 0
      distances.zipWithIndex.foreach {
        case ((i, j), t) =>
          if (t == iterations) {
            part1Solution = junctionBoxes.indices
              .foldLeft(Map.empty[Int, Int]) { (acc, i) =>
                acc.updatedWith(find(i)) {
                  case Some(value) => Some(value + 1)
                  case None        => Some(1)
                }
              }
              .values
              .toSeq
              .sorted
              .takeRight(3)
              .product
          }
          if (find(i) != find(j)) {
            connections += 1
            if (connections == junctionBoxes.length - 1) {
              part2Solution = junctionBoxes(i).x * junctionBoxes(j).x
            } else {
              union(i, j)
            }
          }
      }

      solved = true
      (part1Solution, part2Solution)
    }
  }
}

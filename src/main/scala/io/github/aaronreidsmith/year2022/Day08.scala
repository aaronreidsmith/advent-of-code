package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, Solution}

import scala.collection.SortedMap
import scala.io.Source

object Day08 extends Solution {
  type I  = SortedMap[Point, Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): SortedMap[Point, Int] = {
    val pairs = for {
      (line, row) <- file.getLines().zipWithIndex
      (char, col) <- line.zipWithIndex
    } yield Point(row, col) -> char.asDigit
    SortedMap.from(pairs)
  }

  override def part1(input: SortedMap[Point, Int]): Int = input.foldLeft(0) {
    case (acc, (position, treeHeight)) =>
      val (left, right, up, down) = treesOfInterest(input, position)
      if (
        isEdge(input, position) ||
        left.forall(_ < treeHeight) ||
        right.forall(_ < treeHeight) ||
        up.forall(_ < treeHeight) ||
        down.forall(_ < treeHeight)
      ) {
        acc + 1
      } else {
        acc
      }
  }

  override def part2(input: SortedMap[Point, Int]): Int = {
    def viewingDistance(trees: Vector[Int], height: Int): Int = if (trees.forall(_ < height)) {
      trees.size
    } else {
      trees.takeWhile(_ < height).size + 1
    }

    input.foldLeft(Int.MinValue) {
      case (acc, (position, treeHeight)) if !isEdge(input, position) =>
        val (left, right, up, down) = treesOfInterest(input, position)
        val scenicScore = {
          viewingDistance(left.reverse, treeHeight) *
            viewingDistance(right, treeHeight) *
            viewingDistance(up, treeHeight) *
            viewingDistance(down.reverse, treeHeight)
        }
        if (scenicScore > acc) scenicScore else acc
      case (acc, _) => acc
    }
  }

  private def isEdge(grid: SortedMap[Point, Int], position: Point): Boolean = {
    Seq(position.up, position.right, position.left, position.down).exists(point => !grid.contains(point))
  }

  private def treesOfInterest(
      grid: SortedMap[Point, Int],
      point: Point
  ): (Vector[Int], Vector[Int], Vector[Int], Vector[Int]) = {
    grid.foldLeft((Vector.empty[Int], Vector.empty[Int], Vector.empty[Int], Vector.empty[Int])) {
      case ((left, right, up, down), (other, height)) if other != point && other.sameRowAs(point) =>
        if (other.isLeftOf(point)) (left :+ height, right, up, down) else (left, right :+ height, up, down)
      case ((left, right, up, down), (other, height)) if other != point && other.sameColumnAs(point) =>
        if (other.isAbove(point)) (left, right, up :+ height, down) else (left, right, up, down :+ height)
      case (acc, _) => acc
    }
  }
}

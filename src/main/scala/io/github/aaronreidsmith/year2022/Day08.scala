package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, using}

import scala.collection.SortedMap
import scala.io.Source

object Day08 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day08.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): SortedMap[Point, Int] = {
    val pairs = for {
      (line, row) <- file.getLines().zipWithIndex
      (char, col) <- line.zipWithIndex
    } yield Point(row, col) -> char.asDigit
    SortedMap.from(pairs)
  }

  protected[year2022] def part1(input: SortedMap[Point, Int]): Int = input.foldLeft(0) {
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
    case (acc, _) => acc
  }

  protected[year2022] def part2(input: SortedMap[Point, Int]): Int = {
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
            viewingDistance(up.reverse, treeHeight) *
            viewingDistance(down, treeHeight)
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
      case ((left, right, up, down), (other, height)) =>
        if (other != point && other.x == point.x && other.y < point.y) {
          (left :+ height, right, up, down)
        } else if (other != point && other.x == point.x && other.y > point.y) {
          (left, right :+ height, up, down)
        } else if (other != point && other.y == point.y && other.x < point.x) {
          (left, right, up :+ height, down)
        } else if (other != point && other.y == point.y && other.x > point.x) {
          (left, right, up, down :+ height)
        } else {
          (left, right, up, down)
        }
      case (acc, _) => acc
    }
  }
}
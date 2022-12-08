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
      if (
        isEdge(input, position) ||
        treesToLeft(input, position).forall(_ < treeHeight) ||
        treesToRight(input, position).forall(_ < treeHeight) ||
        treesAbove(input, position).forall(_ < treeHeight) ||
        treesBelow(input, position).forall(_ < treeHeight)
      ) {
        acc + 1
      } else {
        acc
      }
    case (acc, _) => acc
  }

  protected[year2022] def part2(input: SortedMap[Point, Int]): Int = input.foldLeft(Int.MinValue) {
    case (acc, (position, treeHeight)) if !isEdge(input, position) =>
      val left = {
        val all = treesToLeft(input, position).toVector.reverse
        if (all.forall(_ < treeHeight)) all.size else all.takeWhile(_ < treeHeight).size + 1
      }
      val right = {
        val all = treesToRight(input, position).toVector
        if (all.forall(_ < treeHeight)) all.size else all.takeWhile(_ < treeHeight).size + 1
      }
      val up = {
        val all = treesAbove(input, position).toVector.reverse
        if (all.forall(_ < treeHeight)) all.size else all.takeWhile(_ < treeHeight).size + 1
      }
      val down = {
        val all = treesBelow(input, position).toVector
        if (all.forall(_ < treeHeight)) all.size else all.takeWhile(_ < treeHeight).size + 1
      }
      val scenicScore = left * right * up * down
      if (scenicScore > acc) scenicScore else acc
    case (acc, _) => acc
  }

  private def isEdge(grid: SortedMap[Point, Int], position: Point): Boolean = {
    Seq(position.up, position.right, position.left, position.down).exists(point => !grid.contains(point))
  }

  // TODO: There *has* to be a way to combine all of these into one
  private def treesToLeft(grid: SortedMap[Point, Int], point: Point): Iterator[Int] = grid.iterator.collect {
    case (other, height) if other != point && other.x == point.x && other.y < point.y => height
  }

  private def treesToRight(grid: SortedMap[Point, Int], point: Point): Iterator[Int] = grid.iterator.collect {
    case (other, height) if other != point && other.x == point.x && other.y > point.y => height
  }

  private def treesAbove(grid: SortedMap[Point, Int], point: Point): Iterator[Int] = grid.iterator.collect {
    case (other, height) if other != point && other.y == point.y && other.x < point.x => height
  }

  private def treesBelow(grid: SortedMap[Point, Int], point: Point): Iterator[Int] = grid.iterator.collect {
    case (other, height) if other != point && other.y == point.y && other.x > point.x => height
  }
}

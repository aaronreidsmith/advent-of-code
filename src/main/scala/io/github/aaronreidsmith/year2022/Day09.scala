package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, Solution, using}

import scala.io.Source

object Day09 extends Solution {
  type I = String
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2022, Day 9")
    val input = using("2022/day09.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2022] def parseInput(file: Source): String = {
    val parsed = new StringBuilder
    file.getLines().foreach { line =>
      val split     = line.split(' ')
      val direction = split.head
      val times     = split.last.toInt
      parsed.append(direction * times)
    }
    parsed.mkString
  }

  override protected[year2022] def part1(input: String): Int = solution(input, 2)
  override protected[year2022] def part2(input: String): Int = solution(input, 10)

  private def solution(input: String, numKnots: Int): Int = {
    def moveHead(point: Point, direction: Char): Point = direction match {
      case 'U' => point.up
      case 'R' => point.right
      case 'L' => point.left
      case 'D' => point.down
      case _   => throw new IllegalArgumentException
    }

    def moveKnot(knot: Point, other: Point): Point = {
      if (other == knot || knot.neighbors.contains(other)) {
        knot
      } else if (other.sameColumnAs(knot)) {
        if (other.isAbove(knot)) knot.up else knot.down
      } else if (other.sameRowAs(knot)) {
        if (other.isLeftOf(knot)) knot.left else knot.right
      } else { // Diagonal
        if (other.isRightOf(knot)) {
          if (other.isAbove(knot)) knot.right.up else knot.right.down
        } else {
          if (other.isAbove(knot)) knot.left.up else knot.left.down
        }
      }
    }

    val initialKnots = (0 until numKnots).map(_ -> Point.zero).toMap
    val (_, visited) = input.foldLeft((initialKnots, Set(Point.zero))) {
      case ((knots, seen), direction) =>
        val newHead = moveHead(knots(0), direction)
        val newState = (1 until numKnots).foldLeft(Map(0 -> newHead)) {
          case (acc, knot) => acc.updated(knot, moveKnot(knots(knot), acc(knot - 1)))
        }
        (newState, seen + newState(numKnots - 1))
      case (acc, _) => acc
    }
    visited.size
  }
}

package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, using}

import scala.io.Source

object Day09 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day09.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): String = {
    val parsed = new StringBuilder
    file.getLines().foreach { line =>
      val split     = line.split(' ')
      val direction = split.head
      val times     = split.last.toInt
      parsed.append(direction * times)
    }
    parsed.mkString
  }

  protected[year2022] def part1(input: String): Int = {
    val (_, _, visited) = input.foldLeft((Point.zero, Point.zero, Set(Point.zero))) {
      case ((head, tail, seen), direction) =>
        val newHead = moveHead(head, direction)
        val newTail = move(tail, newHead)
        (newHead, newTail, seen + newTail)
      case (acc, _) => acc
    }
    visited.size
  }

  protected[year2022] def part2(input: String): Int = {
    val initialKnots = (0 to 9).map(_ -> Point.zero).toMap
    val (_, visited) = input.foldLeft((initialKnots, Set(Point.zero))) {
      case ((knots, seen), direction) =>
        val newHead = moveHead(knots(0), direction)
        val newState = (1 to 9).foldLeft(Map(0 -> newHead)) {
          case (acc, knot) => acc.updated(knot, move(knots(knot), acc(knot - 1)))
          case (acc, _)    => acc
        }
        (newState, seen + newState(9))
      case (acc, _) => acc
    }
    visited.size
  }

  private def moveHead(point: Point, direction: Char): Point = direction match {
    case 'U' => point.up
    case 'R' => point.right
    case 'L' => point.left
    case 'D' => point.down
    case _   => throw new IllegalArgumentException
  }

  private def move(point: Point, other: Point): Point = {
    if (other == point || point.neighbors.contains(other)) {
      point
    } else if (other.x == point.x) { // Same row...
      if (other.y < point.y) point.up else point.down
    } else if (other.y == point.y) { // Same column
      if (other.x < point.x) point.left else point.right
    } else { // Diagonal
      (other.x > point.x, other.y > point.y) match {
        case (true, true)   => point.right.down
        case (true, false)  => point.right.up
        case (false, true)  => point.left.down
        case (false, false) => point.left.up
      }
    }
  }
}

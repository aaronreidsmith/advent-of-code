package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/18l0qtr/comment/kdv18dn
object Day18 extends Solution {
  type I  = List[String]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[String] = file.getLines().toList

  override def part1(input: List[String]): Long = {
    val mapped = input.map { line =>
      val Array(direction, steps, _*) = line.split(' '): @unchecked
      val point = direction match {
        case "U" => Point(0, -1)
        case "D" => Point(0, 1)
        case "L" => Point(-1, 0)
        case "R" => Point(1, 0)
        case _   => throw new IllegalArgumentException
      }
      (point, steps.toDouble)
    }
    solution(mapped)
  }

  override def part2(input: List[String]): Long = {
    val mapped = input.map { line =>
      val color = line.split(' ').last.filter(_.isLetterOrDigit)
      val point = color.last match {
        case '0' => Point(1, 0)
        case '1' => Point(0, 1)
        case '2' => Point(-1, 0)
        case '3' => Point(0, -1)
        case _   => throw new IllegalArgumentException
      }
      (point, java.lang.Long.parseLong(color.init, 16).toDouble)
    }
    solution(mapped)
  }

  private def solution(input: List[(Point, Double)]): Long = input
    .foldLeft((0d, 1d)) {
      case ((position, answer), (point, steps)) =>
        val updatedPosition = position + point.x * steps
        (updatedPosition, answer + point.y * steps * updatedPosition + steps / 2)
    }
    ._2
    .toLong
}

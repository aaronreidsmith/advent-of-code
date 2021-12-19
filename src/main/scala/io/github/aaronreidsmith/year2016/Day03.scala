package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2016/day03.txt")
    val inputList = input
      .getLines()
      .map { line =>
        val Array(a, b, c, _*) = line.trim.split("\\s+")
        List(a.toInt, b.toInt, c.toInt)
      }
      .toList
    input.close()

    val part1 = inputList.count {
      case a :: b :: c :: _ => isTriangle(a, b, c)
      case _                => false
    }
    println(s"Part 1: $part1")

    val part2 = inputList.transpose.foldLeft(0) { (acc, column) =>
      acc + column.grouped(3).count {
        case a :: b :: c :: _ => isTriangle(a, b, c)
        case _                => false
      }
    }
    println(s"Part 2: $part2")
  }

  private def isTriangle(a: Int, b: Int, c: Int): Boolean = (a + b) > c && (a + c) > b && (b + c) > a
}

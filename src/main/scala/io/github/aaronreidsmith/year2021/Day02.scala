package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day02 {
  private val forward = "^forward (\\d+)$".r
  private val down    = "^down (\\d+)$".r
  private val up      = "^up (\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day02.txt"))(_.getLines().toList)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private def part1(instructions: List[String]): Int = {
    val (finalRow, finalCol) = instructions.foldLeft((0, 0)) {
      case ((row, col), forward(x)) => (row, col + x.toInt)
      case ((row, col), down(x))    => (row + x.toInt, col)
      case ((row, col), up(x))      => (row - x.toInt, col)
      case (acc, _)                 => acc
    }
    finalRow * finalCol
  }

  private def part2(instructions: List[String]): Int = {
    val (finalRow, finalCol, _) = instructions.foldLeft((0, 0, 0)) {
      case ((row, col, aim), forward(x)) => (row + aim * x.toInt, col + x.toInt, aim)
      case ((row, col, aim), down(x))    => (row, col, aim + x.toInt)
      case ((row, col, aim), up(x))      => (row, col, aim - x.toInt)
      case (acc, _)                      => acc
    }
    finalRow * finalCol
  }
}

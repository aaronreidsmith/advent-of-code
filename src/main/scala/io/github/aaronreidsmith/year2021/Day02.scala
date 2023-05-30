package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.*

import scala.io.Source

object Day02 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  // Only want to compile these once
  private val forward = """^forward (\d+)$""".r
  private val down    = """^down (\d+)$""".r
  private val up      = """^up (\d+)$""".r

  override def parseInput(file: Source): List[String] = file.getLines().toList

  override def part1(input: List[String]): Int = {
    val finalPos = input.foldLeft(Point.zero) {
      case (point, forward(x)) => point.move(East, x.toInt)
      case (point, down(x))    => point.move(South, x.toInt)
      case (point, up(x))      => point.move(North, x.toInt)
      case (point, _)          => point
    }
    finalPos.x * finalPos.y
  }

  override def part2(input: List[String]): Int = {
    val (finalPos, _) = input.foldLeft((Point.zero, 0)) {
      case ((point, aim), forward(x)) => (Point(point.x + aim * x.toInt, point.y + x.toInt), aim)
      case ((point, aim), down(x))    => (point, aim + x.toInt)
      case ((point, aim), up(x))      => (point, aim - x.toInt)
      case (acc, _)                   => acc
    }
    finalPos.x * finalPos.y
  }

//  def main(args: Array[String]): Unit = {
//    val input = Using.resource(Source.fromResource("2021/day02.txt"))(_.getLines().toList)
//    println(s"Part 1: ${part1(input)}")
//    println(s"Part 2: ${part2(input)}")
//  }
//
//  private def part1(instructions: List[String]): Int = {
//    val (finalRow, finalCol) = instructions.foldLeft((0, 0)) {
//      case ((row, col), forward(x)) => (row, col + x.toInt)
//      case ((row, col), down(x))    => (row + x.toInt, col)
//      case ((row, col), up(x))      => (row - x.toInt, col)
//      case (acc, _)                 => acc
//    }
//    finalRow * finalCol
//  }
//
//  private def part2(instructions: List[String]): Int = {
//    val (finalRow, finalCol, _) = instructions.foldLeft((0, 0, 0)) {
//      case ((row, col, aim), forward(x)) => (row + aim * x.toInt, col + x.toInt, aim)
//      case ((row, col, aim), down(x))    => (row, col, aim + x.toInt)
//      case ((row, col, aim), up(x))      => (row, col, aim - x.toInt)
//      case (acc, _)                      => acc
//    }
//    finalRow * finalCol
//  }
}

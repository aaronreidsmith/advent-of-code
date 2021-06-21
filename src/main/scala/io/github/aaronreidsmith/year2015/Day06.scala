package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day06 {
  private val turnOn  = "^turn on (\\d+),(\\d+) through (\\d+),(\\d+)$".r("x1", "y1", "x2", "y2")
  private val turnOff = "^turn off (\\d+),(\\d+) through (\\d+),(\\d+)$".r("x1", "y1", "x2", "y2")
  private val toggle  = "^toggle (\\d+),(\\d+) through (\\d+),(\\d+)$".r("x1", "y1", "x2", "y2")

  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("2015/day06.txt")
    val instructions = input.getLines().toList
    input.close()

    val part1Lights = Array.fill(1000)(Array.fill(1000)(false))
    val part2Lights = Array.fill(1000)(Array.fill(1000)(0))

    @tailrec
    def part1(instructions: List[String]): Int = instructions match {
      case Nil => part1Lights.foldLeft(0)(_ + _.count(_ == true))
      case current :: rest =>
        current match {
          case turnOn(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } part1Lights(x)(y) = true
          case turnOff(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } part1Lights(x)(y) = false
          case toggle(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } {
              val current = part1Lights(x)(y)
              part1Lights(x)(y) = !current
            }
          case _ => throw new IllegalArgumentException
        }
        part1(rest)
    }

    @tailrec
    def part2(instructions: List[String]): Int = instructions match {
      case Nil => part2Lights.foldLeft(0)(_ + _.sum)
      case current :: rest =>
        current match {
          case turnOn(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } part2Lights(x)(y) += 1
          case turnOff(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } {
              val current = part2Lights(x)(y)
              part2Lights(x)(y) = if (current <= 0) 0 else current - 1
            }
          case toggle(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } part2Lights(x)(y) += 2
          case _ => throw new IllegalArgumentException
        }
        part2(rest)
    }

    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: ${part2(instructions)}")
  }
}

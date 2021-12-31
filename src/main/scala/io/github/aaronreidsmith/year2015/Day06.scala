package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.io.Source

object Day06 {
  def main(args: Array[String]): Unit = {
    val instructions = using("2015/day06.txt")(_.getLines().toList)
    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: ${part2(instructions)}")
  }

  private val turnOn  = "^turn on (\\d+),(\\d+) through (\\d+),(\\d+)$".r
  private val turnOff = "^turn off (\\d+),(\\d+) through (\\d+),(\\d+)$".r
  private val toggle  = "^toggle (\\d+),(\\d+) through (\\d+),(\\d+)$".r

  private[year2015] def part1(input: List[String]): Int = {
    val lights = Array.fill(1000)(Array.fill(1000)(false))

    @tailrec
    def helper(instructions: List[String]): Int = instructions match {
      case Nil => lights.foldLeft(0)(_ + _.count(_ == true))
      case current :: rest =>
        current match {
          case turnOn(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } lights(x)(y) = true
          case turnOff(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } lights(x)(y) = false
          case toggle(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } {
              val current = lights(x)(y)
              lights(x)(y) = !current
            }
          case _ => throw new IllegalArgumentException
        }
        helper(rest)
    }

    helper(input)
  }

  private[year2015] def part2(input: List[String]): Int = {
    val lights = Array.fill(1000)(Array.fill(1000)(0))

    @tailrec
    def helper(instructions: List[String]): Int = instructions match {
      case Nil => lights.foldLeft(0)(_ + _.sum)
      case current :: rest =>
        current match {
          case turnOn(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } lights(x)(y) += 1
          case turnOff(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } {
              val current = lights(x)(y)
              lights(x)(y) = if (current <= 0) 0 else current - 1
            }
          case toggle(x1, y1, x2, y2) =>
            for {
              x <- x1.toInt to x2.toInt
              y <- y1.toInt to y2.toInt
            } lights(x)(y) += 2
          case _ => throw new IllegalArgumentException
        }
        helper(rest)
    }

    helper(input)
  }
}

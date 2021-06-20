package io.github.aaronreidsmith.year2017

import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val input   = Source.fromResource("2017/day01.txt")
    val captcha = input.getLines().mkString.map(_.toString.toInt)
    input.close()

    val part1 = (captcha :+ captcha.head).sliding(2).foldLeft(0) {
      case (acc, Vector(a, b)) => acc + (if (a == b) a else 0)
    }
    println(s"Part 1: $part1")

    val stepSize = captcha.length / 2
    val rotated  = captcha.drop(stepSize) ++: captcha.take(stepSize)
    val part2 = captcha.zip(rotated).foldLeft(0) {
      case (acc, (a, b)) => acc + (if (a == b) a else 0)
    }
    println(s"Part 2: $part2")
  }
}

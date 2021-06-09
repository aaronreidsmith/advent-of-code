package io.github.aaronreidsmith.year2017

import scala.io.Source
import scala.language.implicitConversions

object Day13 {
  case class Scanner(depth: Int, range: Int) {
    def detected(delay: Int): Boolean = (depth + delay) % (2 * range - 2) == 0
    val severity: Int                 = if (detected(0)) depth * range else 0
  }

  implicit class FancyString(str: String) {
    private val scanner = "^(\\d+): (\\d+)$".r("depth", "range")

    def toScanner: Scanner = str match {
      case scanner(depth, range) => Scanner(depth.toInt, range.toInt)
      case _                     => throw new IllegalArgumentException
    }
  }

  def main(args: Array[String]): Unit = {
    val input    = Source.fromResource("2017/day13.txt")
    val scanners = input.getLines().map(_.toScanner).toList
    input.close()

    println(s"Part 1: ${scanners.map(_.severity).sum}")

    def isSafe(delay: Int): Boolean = scanners.forall(!_.detected(delay))
    println(s"Part 2: ${Stream.from(0).find(isSafe).get}")
  }
}

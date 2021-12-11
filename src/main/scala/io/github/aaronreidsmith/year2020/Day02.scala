package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith._

object Day02 {
  private val passwordEntry = "^(\\d+)-(\\d+) ([a-z]): ([a-z]+)$".r

  def main(args: Array[String]): Unit = {
    val input = using("2020/day02.txt")(_.getLines().toList)
    val part1 = input.count {
      case passwordEntry(rangeStart, rangeEnd, target, password) =>
        val targetCount = password.count(_ == target.head)
        rangeStart.toInt <= targetCount && targetCount <= rangeEnd.toInt
      case _ => false
    }
    println(s"Part 1: $part1")
    val part2 = input.count {
      case passwordEntry(positionOne, positionTwo, target, password) =>
        val indexOne   = positionOne.toInt - 1
        val indexTwo   = positionTwo.toInt - 1
        val targetChar = target.head
        password(indexOne) == targetChar ^ password(indexTwo) == targetChar
      case _ => false
    }
    println(s"Part 2: $part2")
  }
}

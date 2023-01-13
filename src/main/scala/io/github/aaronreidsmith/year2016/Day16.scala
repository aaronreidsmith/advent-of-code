package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends Solution {
  type I  = String
  type O1 = String
  type O2 = String

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): String     = checksum(modifiedDragon(input, 272))
  override def part2(input: String): String     = checksum(modifiedDragon(input, 35651584))

  @tailrec
  private def modifiedDragon(a: String, targetLength: Int): String = if (a.length >= targetLength) {
    a.take(targetLength)
  } else {
    val b = a.reverse.replaceAll("1", "_").replaceAll("0", "1").replaceAll("_", "0")
    modifiedDragon(s"${a}0$b", targetLength)
  }

  @tailrec
  private def checksum(input: String): String = if (input.length % 2 != 0) {
    input
  } else {
    val newInput = input
      .grouped(2)
      .foldLeft(new StringBuilder) { (acc, pair) =>
        val next = if (pair.head == pair.last) "1" else "0"
        acc ++= next
      }
      .mkString
    checksum(newInput)
  }
}

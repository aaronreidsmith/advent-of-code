package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec

object Day16 extends Solution {
  type I  = String
  type O1 = String
  type O2 = String

  def run(): Unit = {
    println("Year 2016, Day 16")
    val input = "10001110011110000"
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def part1(input: String): String = checksum(modifiedDragon(input, 272))
  override protected[year2016] def part2(input: String): String = checksum(modifiedDragon(input, 35651584))

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

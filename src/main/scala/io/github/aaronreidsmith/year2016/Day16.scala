package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    val input = "10001110011110000"
    println(s"Part 1: ${checksum(modifiedDragon(input, 272))}")
    println(s"Part 2: ${checksum(modifiedDragon(input, 35651584))}")
  }

  @tailrec
  def modifiedDragon(a: String, targetLength: Int): String = if (a.length >= targetLength) {
    a.take(targetLength)
  } else {
    val b = a.reverse.replaceAll("1", "_").replaceAll("0", "1").replaceAll("_", "0")
    modifiedDragon(s"${a}0$b", targetLength)
  }

  @tailrec
  def checksum(input: String): String = if (input.length % 2 != 0) {
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

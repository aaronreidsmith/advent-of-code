package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val input = 1321131112
    println(s"Part 1: ${solution(input.toString, 40)}")
    println(s"Part 2: ${solution(input.toString, 50)}")
  }

  @tailrec
  private def solution(currentNum: String, iterations: Int, currentIteration: Int = 0): Int =
    if (currentIteration >= iterations) {
      currentNum.length
    } else {
      val builder = new StringBuilder
      var current = currentNum.head
      var count   = 1

      val lastIndex = currentNum.tail.length - 1
      currentNum.tail.zipWithIndex.foreach {
        case (digit, index) =>
          if (digit == current) {
            count += 1
          } else {
            builder ++= s"$count$current"
            current = digit
            count = 1
            if (index == lastIndex) {
              builder ++= s"$count$current"
            }
          }
      }
      solution(builder.toString(), iterations, currentIteration + 1)
    }
}

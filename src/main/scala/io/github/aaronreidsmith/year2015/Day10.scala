package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec

object Day10 extends Solution {
  type I  = Int
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2015, Day 10")
    val input = 1321131112
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2015] def part1(input: Int): Int = solution(input.toString, 40)
  override protected[year2015] def part2(input: Int): Int = solution(input.toString, 50)

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

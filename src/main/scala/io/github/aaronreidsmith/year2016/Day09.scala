package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/5hbygy/comment/dazb5db
object Day09 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Long

  def run(): Unit = {
    println("Year 2016, Day 9")
    val input = using("2016/day09.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): String = file.mkString

  override protected[year2016] def part1(input: String): Int = {
    @tailrec
    def decompress(content: List[Char], count: Int = 0): Int = content match {
      case Nil => count
      case '(' :: _ =>
        val markerIndex = content.indexOf(')')
        val marker      = content.slice(1, markerIndex)
        val index       = marker.indexOf('x')
        val len         = marker.take(index).mkString.toInt
        val times       = marker.slice(index + 1, marker.size).mkString.toInt
        decompress(content.slice(markerIndex + len + 1, content.size), count + (times * len))
      case _ :: tail => decompress(tail, count + 1)
    }

    decompress(input.toList)
  }

  override protected[year2016] def part2(input: String): Long = {
    def decompress(content: List[Char], count: Long = 0L): Long = content match {
      case Nil => count
      case '(' :: _ =>
        val markerIndex = content.indexOf(')')
        val marker      = content.slice(1, markerIndex)
        val index       = marker.indexOf('x')
        val len         = marker.take(index).mkString.toInt
        val times       = marker.slice(index + 1, marker.size).mkString.toInt
        decompress(
          content.slice(markerIndex + len + 1, content.size),
          count + (times * decompress(content.slice(markerIndex + 1, markerIndex + len + 1)))
        )
      case _ :: tail => decompress(tail, count + 1)
    }

    decompress(input.toList)
  }
}

package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day11 extends Solution {
  type I  = List[Long]
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): List[Long] = {
    file.mkString.split(' ').toList.map(_.toLong)
  }

  override def part1(input: List[Long]): Int = {
    @tailrec
    def helper(state: List[Long], iteration: Int = 0): Int = {
      if (iteration >= 25) {
        state.size
      } else {
        val next = state.flatMap {
          case 0 => List(1L)
          case num =>
            val str = num.toString
            if (str.length % 2 == 0) {
              val left  = str.take(str.length / 2).toLong
              val right = str.drop(str.length / 2).toLong
              List(left, right)
            } else {
              List(num * 2024)
            }
        }
        helper(next, iteration + 1)
      }
    }

    helper(input)
  }

  override def part2(input: List[Long]): Long = {
    val cache = mutable.Map.empty[(Long, Int), Long]
    def count(stone: Long, iteration: Int = 75): Long = cache.getOrElseUpdate(
      (stone, iteration), {
        if (iteration == 0) {
          1L
        } else if (stone == 0) {
          count(1, iteration - 1)
        } else {
          val length = math.floor(math.log10(stone.toDouble)).toLong + 1
          if (length % 2 == 0) {
            count(stone / math.pow(10, (length / 2).toDouble).toLong, iteration - 1) +
              count(stone % math.pow(10, (length / 2).toDouble).toLong, iteration - 1)
          } else {
            count(stone * 2024, iteration - 1)
          }
        }
      }
    )

    input.foldLeft(0L)((acc, stone) => acc + count(stone))
  }
}

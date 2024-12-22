package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

@Slow(part2 = true)
object Day22 extends Solution {
  type I  = List[Long]
  type O1 = Long
  type O2 = Int

  extension (n: Long) {
    private def mix(other: Long): Long = {
      n ^ other
    }

    private def prune: Long = {
      n % 16777216
    }

    def next: Long = {
      val a = n.mix(n * 64).prune
      val b = a.mix(a / 32).prune
      b.mix(b * 2048).prune
    }
  }

  override def parseInput(file: Source): List[Long] = {
    file.getLines().toList.map(_.toLong)
  }

  override def part1(input: List[Long]): Long = {
    @tailrec
    def helper(state: List[Long], n: Int): Long = {
      if (n >= 2000) {
        state.sum
      } else {
        helper(state.map(_.next), n + 1)
      }
    }

    helper(input, 0)
  }

  override def part2(input: List[Long]): Int = {
    val bananas = mutable.Map.empty[(Int, Int, Int, Int), Int].withDefaultValue(0)
    input.foreach { num =>
      val nums  = Iterator.iterate(num)(_.next).take(2001).toList
      val diffs = nums.sliding(2).collect { case Seq(a, b) => ((b % 10) - (a % 10)).toInt }.toVector
      nums.indices.dropRight(4).foldLeft(Set.empty[(Int, Int, Int, Int)]) { (seen, i) =>
        val Seq(a, b, c, d) = diffs.slice(i, i + 4): @unchecked
        val pattern         = (a, b, c, d)
        if (!seen.contains(pattern)) {
          bananas(pattern) += (nums(i + 4) % 10).toInt
          seen + pattern
        } else {
          seen
        }
      }
    }
    bananas.values.max
  }
}

package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.Solution

import scala.collection.immutable.NumericRange
import scala.io.Source

object Day02 extends Solution {
  type I  = Seq[NumericRange[Long]]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): Seq[NumericRange[Long]] = {
    file.mkString.trim.split(',').toSeq.map { str =>
      val Array(min, max, _*) = str.split('-'): @unchecked
      Range.Long.inclusive(min.toLong, max.toLong, 1L)
    }
  }

  override def part1(input: Seq[NumericRange[Long]]): Long = {
    solution(input) { num =>
      val str    = num.toString
      val length = str.length
      length % 2 == 0 && str.take(length / 2) == str.drop(length / 2)
    }
  }

  override def part2(input: Seq[NumericRange[Long]]): Long = {
    solution(input) { num =>
      val str = num.toString
      val isInvalid = (1 to str.length / 2).exists { i =>
        val grouped = str.grouped(i).toSeq
        grouped.forall(_.length == i) && grouped.count(_ == str.take(i)) == str.length / i
      }
      isInvalid
    }
  }

  private def solution(input: Seq[NumericRange[Long]])(isInvalid: Long => Boolean): Long = {
    input.foldLeft(0L) { (sum, range) =>
      sum + range.foldLeft(0L) {
        case (acc, num) if isInvalid(num) => acc + num
        case (acc, _)                     => acc
      }
    }
  }
}

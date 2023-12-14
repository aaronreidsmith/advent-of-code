package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/18h940b/comment/kd97vdk/?utm_source=share&utm_medium=web2x&context=3
object Day13 extends Solution {
  type Pattern = Vector[Vector[Char]]
  type I       = Vector[Pattern]
  type O1      = Int
  type O2      = Int

  override def parseInput(file: Source): Vector[Pattern] = file.mkString.trim.split("\n\n").toVector.map { block =>
    block.split('\n').toVector.map(_.toVector)
  }

  override def part1(input: Vector[Pattern]): Int = input.foldLeft(0) { (acc, pattern) =>
    symmetry(pattern) match {
      case Some(horizontal) => acc + (100 * horizontal)
      case None             => acc + symmetry(pattern.transpose).getOrElse(0)
    }
  }

  override def part2(input: Vector[Pattern]): Int = input.foldLeft(0) { (acc, pattern) =>
    symmetrySmudged(pattern) match {
      case Some(horizontal) => acc + (100 * horizontal)
      case None             => acc + symmetrySmudged(pattern.transpose).getOrElse(0)
    }
  }

  private def symmetry(pattern: Pattern): Option[Int] = {
    val n = pattern.length
    (1 until n).find { i =>
      (0 until i.min(n - i)).forall(j => pattern(i - j - 1) == pattern(i + j))
    }
  }

  private def symmetrySmudged(pattern: Pattern): Option[Int] = {
    def hamming(x: Vector[Char], y: Vector[Char]): Int = x.zip(y).count(_ != _)

    val n = pattern.length
    (1 until n).find { i =>
      (0 until i.min(n - i)).foldLeft(0)((acc, j) => acc + hamming(pattern(i - j - 1), pattern(i + j))) == 1
    }
  }
}

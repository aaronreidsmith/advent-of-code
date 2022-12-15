package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/7l78eb/2017_day_21_solutions/drk8egp
// No way I am smart enough to figure this out myself
object Day21 extends Solution(2017, 21) {
  private type Rules = Map[Square[Char], Square[Char]]
  type I             = Rules
  type O1            = Int
  type O2            = Int

  private[year2017] case class Square[T](twoDimensional: Vector[Vector[T]]) {
    val size: Int = twoDimensional.length

    def flip: Square[T]   = Square(twoDimensional.reverse)
    def rotate: Square[T] = Square(twoDimensional.reverse.transpose)
    def split(smallerSize: Int): Square[Square[T]] = Square(
      twoDimensional
        .grouped(smallerSize)
        .toVector
        .map(_.map(_.grouped(smallerSize).toVector).transpose.map(Square(_)))
    )

    lazy val permutations: Set[Square[T]] = Set(
      this,
      this.rotate,
      this.rotate.rotate,
      this.rotate.rotate.rotate,
      this.flip,
      this.flip.rotate,
      this.flip.rotate.rotate,
      this.flip.rotate.rotate.rotate
    )

    def flatten[R](implicit asSquare: T => Square[R]): Square[R] = Square(
      twoDimensional.flatMap { row =>
        row.map(_.twoDimensional).transpose.map(_.flatten)
      }
    )

    def map[R](f: T => R): Square[R] = Square(twoDimensional.map(_.map(f)))

    def flatMap[R](f: T => Square[R]): Square[R] = this.map(f).flatten
  }

  private object Square {
    def apply(twoDimensional: Array[Array[Char]]): Square[Char] = Square(twoDimensional.map(_.toVector).toVector)
    def apply(flat: String): Square[Char]                       = Square(flat.split('/').toVector.map(_.toVector))
  }

  override protected[year2017] def parseInput(file: Source): Rules = {
    val rule = "^(.*) => (.*)$".r
    file
      .getLines()
      .flatMap {
        case rule(input, output) => Square(input).permutations.map(_ -> Square(output))
      }
      .toMap
  }

  override protected[year2017] def part1(input: Rules): Int = solution(input, 5)
  override protected[year2017] def part2(input: Rules): Int = solution(input, 18)

  private def solution(rules: Rules, iterations: Int): Int = {
    val initial = Square(".#./..#/###")
    Iterator
      .iterate(initial) { grid =>
        val smallSquareSize = if (grid.size % 2 == 0) 2 else 3
        grid.split(smallSquareSize).flatMap(rules)
      }
      .slice(iterations, iterations + 1)
      .toSeq
      .head
      .twoDimensional
      .map { row =>
        row
          .groupBy(identity)
          .view
          .mapValues(_.length)
      }
      .foldLeft(0)((acc, counts) => acc + counts('#'))
  }
}

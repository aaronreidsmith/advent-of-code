package io.github.aaronreidsmith.year2017

import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/7l78eb/2017_day_21_solutions/drk8egp
// No way I am smart enough to figure this out myself
object Day21 {
  private val rule = "^(.*) => (.*)$".r

  private case class Square[T](twoDimensional: Vector[Vector[T]]) {
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

  private case class Rule(input: Square[Char], output: Square[Char])

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2017/day21.txt")
    val rules = input
      .getLines()
      .flatMap {
        case rule(input, output) => Square(input).permutations.map(_ -> Square(output))
      }
      .toMap
    input.close()

    val initial = Square(".#./..#/###")

    def solution(iterations: Int): Int = Iterator
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

    println(s"Part 1: ${solution(5)}")
    println(s"Part 2: ${solution(18)}")
  }
}

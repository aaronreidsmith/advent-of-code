package io.github.aaronreidsmith

import scala.io.Source
import scala.language.implicitConversions

package object implicits {
  // So we can unzip a Seq[Point]
  implicit def point2Tuple2(point: Point): (Int, Int) = (point.x, point.y)

  implicit class SourceOps(file: Source) {
    def toCharGrid: Grid[Char] = {
      for {
        (line, row) <- file.getLines().zipWithIndex
        (char, col) <- line.zipWithIndex
      } yield Point(row, col) -> char
    }.toMap

    def toIntGrid: Grid[Int] = {
      for {
        (line, row) <- file.getLines().zipWithIndex
        (char, col) <- line.zipWithIndex
      } yield Point(row, col) -> char.asDigit
    }.toMap
  }

  implicit class StringOps(string: String) {
    def letterOccurrences: Map[Char, Int] = string.toSeq.occurrences
    def toCharGrid: Grid[Char]            = Source.fromString(string).toCharGrid
    def toIntGrid: Grid[Int]              = Source.fromString(string).toIntGrid
  }

  implicit class SeqOps[T](seq: Seq[T]) {
    def occurrences: Map[T, Int] = seq.groupMapReduce(identity)(_ => 1)(_ + _)
  }
}

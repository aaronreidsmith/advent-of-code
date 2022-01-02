package io.github.aaronreidsmith

import scala.io.Source
import scala.language.implicitConversions

package object implicits {
  // So we can unzip a Seq[Point]
  implicit def point2Tuple2(point: Point): (Int, Int) = (point.x, point.y)

  implicit class SourceOps(file: Source) {
    def toGrid: Grid[Char] = {
      for {
        (line, row) <- file.getLines().zipWithIndex
        (char, col) <- line.zipWithIndex
      } yield Point(row, col) -> char
    }.toMap
  }

  implicit class StringOps(string: String) {
    def toGrid: Grid[Char] = Source.fromString(string).toGrid
  }
}

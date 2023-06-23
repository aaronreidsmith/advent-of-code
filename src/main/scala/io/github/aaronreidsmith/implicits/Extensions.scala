package io.github.aaronreidsmith.implicits

import io.github.aaronreidsmith.{Grid, Point}

import scala.collection.mutable
import scala.io.Source

extension [T](it: Iterator[T]) {
  def headOption: Option[T]    = it.nextOption()
  def occurrences: Map[T, Int] = LazyList.from(it).occurrences
}

extension [K, V](map: Map[K, V]) {
  def toMutable: mutable.Map[K, V] = mutable.Map.from(map)
}

extension [T](seq: Seq[T]) {
  def occurrences: Map[T, Int] = seq.groupMapReduce(identity)(_ => 1)(_ + _)
}

extension [T](set: Set[T]) {
  def toMutable: mutable.Set[T] = mutable.Set.from(set)
}

extension (file: Source) {
  def toGrid: Grid[Char] = {
    for {
      (line, row) <- file.getLines().zipWithIndex
      (char, col) <- line.zipWithIndex
    } yield Point(row, col) -> char
  }.toMap
}

extension (string: String) {
  def letterOccurrences: Map[Char, Int] = string.toSeq.occurrences
  def toGrid: Grid[Char]                = Source.fromString(string).toGrid
}

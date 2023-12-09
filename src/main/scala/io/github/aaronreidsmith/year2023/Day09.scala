package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day09 extends Solution {
  type I  = List[List[Long]]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[List[Long]] = file.getLines().toList.map { line =>
    line.split(' ').toList.map(_.toLong)
  }

  override def part1(input: List[List[Long]]): Long = input.foldLeft(0L)(_ + getNext(_))
  override def part2(input: List[List[Long]]): Long = {
    input.foldLeft(0L)((acc, numbers) => acc + getNext(numbers.reverse))
  }

  private def getNext(numbers: List[Long]): Long = if (numbers.forall(_ == 0)) {
    0
  } else {
    val childValues = numbers.init.zip(numbers.tail).map((a, b) => b - a)
    numbers.last + getNext(childValues)
  }
}

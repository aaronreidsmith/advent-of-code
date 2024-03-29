package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day02 extends Solution {
  type I  = Vector[Vector[Int]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Vector[Vector[Int]] = {
    file.getLines().toVector.map(_.split('\t').toVector.map(_.toInt))
  }

  override def part1(input: Vector[Vector[Int]]): Int = {
    input.foldLeft(0)((acc, row) => acc + row.max - row.min)
  }

  override def part2(input: Vector[Vector[Int]]): Int = input.foldLeft(0) { (acc, row) =>
    val evenPair = row
      .combinations(2)
      .collectFirst {
        case Vector(a, b) if a % b == 0 => a / b
        case Vector(a, b) if b % a == 0 => b / a
      }
      .get
    acc + evenPair
  }
}

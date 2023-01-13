package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day15 extends Solution {
  type I  = Vector[Disc]
  type O1 = Int
  type O2 = Int

  case class Disc(number: Int, positions: Int, initialPosition: Int)

  override def parseInput(file: Source): Vector[Disc] = {
    val entry = """^Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.$""".r

    file
      .getLines()
      .collect {
        case entry(number, positions, initialPosition) => Disc(number.toInt, positions.toInt, initialPosition.toInt)
      }
      .toVector
  }

  override def part1(input: Vector[Disc]): Int = solution(input)
  override def part2(input: Vector[Disc]): Int = {
    val updatedInput = input :+ Disc(number = input.length + 1, positions = 11, initialPosition = 0)
    solution(updatedInput)
  }

  private def solution(discs: Vector[Disc]): Int = {
    def position(disc: Disc, dropTime: Int): Int = (disc.number + disc.initialPosition + dropTime) % disc.positions

    LazyList.from(0).find(t => discs.forall(disc => position(disc, t) == 0)).getOrElse(-1)
  }
}

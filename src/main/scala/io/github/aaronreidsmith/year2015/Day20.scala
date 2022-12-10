package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day20 extends Solution(2015, 20) {
  type I  = Int
  type O1 = Int
  type O2 = Int

  override protected[year2015] def parseInput(file: Source): Int = file.mkString.toInt

  override protected[year2015] def part1(input: Int): Int = {
    def presents(houseNumber: Int): Int = (1 to math.sqrt(houseNumber).toInt)
      .foldLeft(Set.empty[Int]) {
        case (acc, n) if houseNumber % n == 0 => acc ++ Set(n, houseNumber / n)
        case (acc, _) => acc
      }
      .sum * 10
    solution(input, presents)
  }

  override protected[year2015] def part2(input: Int): Int = {
    def presents(houseNumber: Int): Int = (1 to 50).foldLeft(0) {
      case (acc, i) if i <= houseNumber && houseNumber % i == 0 => acc + (houseNumber / i)
      case (acc, _) => acc
    } * 11
    solution(input, presents)
  }

  private def solution(input: Int, presents: Int => Int): Int = LazyList
    .from(100_000)
    .collectFirst {
      case houseNumber if presents(houseNumber) >= input => houseNumber
    }
    .getOrElse(-1)
}

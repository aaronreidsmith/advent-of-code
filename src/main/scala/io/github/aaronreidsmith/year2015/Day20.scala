package io.github.aaronreidsmith.year2015

object Day20 {
  def main(args: Array[String]): Unit = {
    val input = 34000000
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2015] def part1(input: Int): Int = {
    def presents(houseNumber: Int): Int = (1 to math.sqrt(houseNumber).toInt)
      .foldLeft(Set.empty[Int]) {
        case (acc, n) if houseNumber % n == 0 => acc ++ Set(n, houseNumber / n)
        case (acc, _) => acc
      }
      .sum * 10
    solution(input, presents)
  }

  private[year2015] def part2(input: Int): Int = {
    def presents(houseNumber: Int): Int = (1 to 50).foldLeft(0) {
      case (acc, i) if i <= houseNumber && houseNumber % i == 0 => acc + (houseNumber / i)
      case (acc, _) => acc
    } * 11
    solution(input, presents)
  }

  private val houseNumbers = LazyList.from(100000)
  private def solution(input: Int, presents: Int => Int): Int = houseNumbers
    .collectFirst {
      case houseNumber if presents(houseNumber) >= input => houseNumber
    }
    .getOrElse(-1)
}

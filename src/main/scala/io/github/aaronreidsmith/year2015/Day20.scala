package io.github.aaronreidsmith.year2015

object Day20 {
  def main(args: Array[String]): Unit = {
    val input = 34000000
    val p1 = LazyList
      .from(100000)
      .collectFirst {
        case houseNumber if part1(houseNumber) >= input => houseNumber
      }
      .get
    println(s"Part 1: $p1")

    val p2 = LazyList
      .from(100000)
      .collectFirst {
        case houseNumber if part2(houseNumber) >= input => houseNumber
      }
      .get
    println(s"Part 2: $p2")
  }

  private def part1(houseNumber: Int): Int = (1 to math.sqrt(houseNumber).toInt)
    .foldLeft(Set.empty[Int]) {
      case (acc, n) if houseNumber % n == 0 => acc ++ Set(n, houseNumber / n)
      case (acc, _) => acc
    }
    .sum * 10

  private def part2(houseNumber: Long): Long = (1 to 50).foldLeft(0L) {
    case (acc, i) if i <= houseNumber && houseNumber % i == 0 => acc + (houseNumber / i)
    case (acc, _) => acc
  } * 11
}

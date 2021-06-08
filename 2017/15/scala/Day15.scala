object Day15 {
  def main(args: Array[String]): Unit = {
    val aStart  = 116L // my input = 116L
    val aFactor = 16807L

    val bStart  = 299L // my input = 299L
    val bFactor = 48271L

    lazy val aGenerator = generator(aStart, aFactor)
    lazy val bGenerator = generator(bStart, bFactor)

    val part1 = aGenerator.take(40000000).zip(bGenerator).count(matching)
    println(s"Part 1: $part1")

    // Our seed is divisible by 4, so we need to skip it
    // https://www.reddit.com/r/adventofcode/comments/nrfqa6/2017_day_15_part_2_scala_works_for_test_input_but/
    val part2 = aGenerator.filter(_ % 4 == 0).slice(1, 5000001).zip(bGenerator.filter(_ % 8 == 0)).count(matching)
    println(s"Part 2: $part2")
  }

  private def generator(previous: Long, factor: Long, divisor: Long = 2147483647): Stream[Long] = {
    val next = (previous * factor) % divisor
    previous #:: generator(next, factor)
  }

  private def lowest16Bits(num: Long): String = {
    val binary = num.toBinaryString
    val padded = if (binary.length < 16) s"0000000000000000$binary" else binary
    padded.takeRight(16)
  }

  private def matching(pair: (Long, Long)): Boolean = {
    val (a, b) = pair
    lowest16Bits(a) == lowest16Bits(b)
  }
}

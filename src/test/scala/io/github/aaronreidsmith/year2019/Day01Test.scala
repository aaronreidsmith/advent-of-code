package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  private val inputs = Seq(
    (14, 2, 2),
    (1969, 654, 966),
    (100756, 33583, 50346)
  )

  "Day01.part1" should "work on example input" in {
    inputs.foreach {
      case (input, part1Answer, _) => Day01.part1(List(input)) shouldBe part1Answer
    }
  }

  "Day01.part2" should "work on example input" in {
    inputs.foreach {
      case (input, _, part2Answer) => Day01.part2(List(input)) shouldBe part2Answer
    }
  }
}

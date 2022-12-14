package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  "Day01" should "work on example input" in {
    Seq(
      (14, 2, 2),
      (1969, 654, 966),
      (100756, 33583, 50346)
    ).foreach {
      case (input, expected1, expected2) =>
        Day01.part1(List(input)) shouldBe expected1
        Day01.part2(List(input)) shouldBe expected2
    }
  }
}

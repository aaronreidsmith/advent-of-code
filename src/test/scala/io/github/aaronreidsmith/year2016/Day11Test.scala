package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day11Test extends BaseTest {
  "Day11.part1" should "work on actual input" in {
    Day11.part1(List(8, 2, 10, 0)) shouldBe 47
  }

  "Day11.part2" should "work on actual input" in {
    Day11.part2(List(12, 2, 10, 0)) shouldBe 71
  }
}

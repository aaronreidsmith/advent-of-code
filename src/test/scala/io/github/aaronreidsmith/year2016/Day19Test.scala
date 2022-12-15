package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day19Test extends BaseTest {
  private val input = 5

  "Day19.part1" should "work on example input" in {
    Day19.part1(input) shouldBe 3
  }

  "Day19.part2" should "work on example input" in {
    Day19.part2(input) shouldBe 2
  }
}

package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day20Test extends BaseTest {
  private val input = 34000000

  "Day20.part1" should "work on actual input" in {
    Day20.part1(input) shouldBe 786240
  }

  "Day20.part2" should "work on actual input" in {
    Day20.part2(input) shouldBe 831600
  }
}

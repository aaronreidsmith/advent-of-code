package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day17Test extends BaseTest {
  private val input = 3

  "Day17.part1" should "work on example input" in {
    Day17.part1(input) shouldBe 638
  }

  "Day17.part2" should "work on example input" in {
    Day17.part2(input) shouldBe 1222153
  }
}

package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day11Test extends BaseTest {
  private val input = "hxbxwxba" // No example input for this problem

  "Day11.part1" should "work on actual input" in {
    Day11.part1(input) shouldBe "hxbxxyzz"
  }

  "Day11.part2" should "work on actual input" in {
    Day11.part2(input) shouldBe "hxcaabcc"
  }
}

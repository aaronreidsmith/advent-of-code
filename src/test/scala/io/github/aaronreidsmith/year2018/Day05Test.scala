package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day05Test extends BaseTest {
  private val input = "dabAcCaCBAcCcaDA"

  "Day05.part1" should "work on example input" in {
    Day05.part1(input) shouldBe 10
  }

  "day05.part2" should "work on example input" in {
    Day05.part2(input) shouldBe 4
  }
}

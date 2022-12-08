package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day06Test extends BaseTest {
  private val input = List(0, 2, 7, 0)

  "Day06.part1" should "work on example input" in {
    Day06.part1(input) shouldBe 5
  }

  "Day06.part2" should "work on example input" in {
    Day06.part2(input) shouldBe 4
  }
}

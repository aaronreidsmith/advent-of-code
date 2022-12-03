package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day03Test extends BaseTest {
  private val input = using("2016/day03.txt")(Day03.parseInput)

  "Day03.part1" should "work on actual input" in {
    Day03.part1(input) shouldBe 983
  }

  "Day03.part2" should "work on actual input" in {
    Day03.part2(input) shouldBe 1836
  }
}

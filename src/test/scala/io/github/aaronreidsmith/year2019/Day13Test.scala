package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day13Test extends BaseTest {
  private val input = using("2019/day13.txt")(Day13.parseInput)

  "Day13.part1" should "work on actual input" in {
    Day13.part1(input) shouldBe 213
  }

  "Day13.part2" should "work on actual input" in {
    Day13.part2(input) shouldBe 11441L
  }
}

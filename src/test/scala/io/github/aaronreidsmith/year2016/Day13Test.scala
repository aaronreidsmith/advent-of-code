package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day13Test extends BaseTest {
  private val input = using("2016/day13.txt")(Day13.parseInput)

  "Day13.part1" should "work on example input" in {
    Day13.part1(input) shouldBe 92
  }

  "Day13.part2" should "work on example input" in {
    Day13.part2(input) shouldBe 124
  }
}

package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, using}

class Day19Test extends BaseTest {
  private val input = using("2017/day19.txt")(Day19.parseInput)

  "Day19" should "work on example input" in {
    Day19.part1(input) shouldBe "ABCDEF"
    Day19.part2(input) shouldBe 38
  }
}

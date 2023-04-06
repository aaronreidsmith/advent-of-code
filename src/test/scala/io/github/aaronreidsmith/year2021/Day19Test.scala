package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day19Test extends BaseTest {
  private val input = using("2021/day19.txt")(Day19.parseInput)

  "Day19.part1" should "work on example input" in {
    Day19.part1(input) shouldBe 79
  }

  "Day19.part2" should "work on example input" in {
    Day19.part2(input) shouldBe 3621
  }
}

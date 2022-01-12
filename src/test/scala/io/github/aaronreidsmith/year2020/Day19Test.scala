package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day19Test extends BaseTest {
  private val (rules, messages) = using("2020/day19.txt")(Day19.parseInput)

  "Day19.part1" should "work on example input" in {
    Day19.part1(rules, messages) shouldBe 3
  }

  "Day19.part2" should "work on example input" in {
    Day19.part2(rules, messages) shouldBe 12
  }
}

package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day13Test extends BaseTest {
  private val rules = using("2015/day13.txt")(Day13.parseInput)

  "Day13.part1" should "work on example input" in {
    Day13.part1(rules) shouldBe 330
  }

  "Day13.part2" should "work on example input" in {
    Day13.part2(rules) shouldBe 286
  }
}

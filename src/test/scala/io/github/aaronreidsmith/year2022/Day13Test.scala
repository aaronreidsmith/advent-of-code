package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day13Test extends BaseTest {
  private val input = using("2022/day13.txt")(Day13.parseInput)

  "Day13.part1" should "work on example input" in {
    Day13.part1(input) shouldBe 13
  }

  "Day13.part2" should "work on example input" in {
    Day13.part2(input) shouldBe 140
  }
}

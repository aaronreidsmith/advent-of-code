package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day13Test extends BaseTest {
  private val input = using("2020/day13.txt")(Day13.parseInput)

  "Day13.part1" should "work on example input" in {
    Day13.part1(input) shouldBe 295
  }

  "Day13.part2" should "work on example input" in {
    Day13.part2(input) shouldBe 1068781L
  }
}

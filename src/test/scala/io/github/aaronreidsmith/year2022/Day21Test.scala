package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day21Test extends BaseTest {
  private val input = using("2022/day21.txt")(Day21.parseInput)

  "Day21.part1" should "work on example input" in {
    Day21.part1(input) shouldBe 152L
  }

  "Day21.part2" should "work on example input" in {
    Day21.part2(input) shouldBe 301L
  }
}

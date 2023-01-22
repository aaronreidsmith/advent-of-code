package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day05Test extends BaseTest {
  private val input = using("2021/day05.txt")(Day05.parseInput)

  "Day05.part1" should "work on example input" in {
    Day05.part1(input) shouldBe 5
  }

  "Day05.part2" should "work on example input" in {
    Day05.part2(input) shouldBe 12
  }
}

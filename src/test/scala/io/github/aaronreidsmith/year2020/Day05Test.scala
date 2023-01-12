package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day05Test extends BaseTest {
  private val input = using("2020/day05.txt")(Day05.parseInput)

  "Day05.part1" should "work on actual input" in {
    Day05.part1(input) shouldBe 828
  }

  "Day05.part2" should "work on actual input"in {
    Day05.part2(input) shouldBe 565
  }
}

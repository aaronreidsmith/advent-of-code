package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day05Test extends BaseTest {
  private val input = using("2022/day05.txt")(Day05.parseInput)

  "Day05.part1" should "work on example input" in {
    Day05.part1(input) shouldBe "CMZ"
  }

  "Day05.part2" should "work on example input" in {
    Day05.part2(input) shouldBe "MCD"
  }
}

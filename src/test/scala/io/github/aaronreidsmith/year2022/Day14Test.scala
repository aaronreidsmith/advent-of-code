package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day14Test extends BaseTest {
  private val input = using("2022/day14.txt")(Day14.parseInput)

  "Day14.part1" should "work on example input" in {
    Day14.part1(input) shouldBe 24
  }

  "Day14.part2" should "work on example input" in {
    Day14.part2(input) shouldBe 93
  }
}

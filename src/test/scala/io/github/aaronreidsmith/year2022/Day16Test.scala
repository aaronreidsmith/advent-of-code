package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day16Test extends BaseTest {
  private val input = using("2022/day16.txt")(Day16.parseInput)

  "Day16.part1" should "work on example input" in {
    Day16.part1(input) shouldBe 1651
  }

  "Day16.part2" should "work on example input" in {
    Day16.part2(input) shouldBe 1707
  }
}

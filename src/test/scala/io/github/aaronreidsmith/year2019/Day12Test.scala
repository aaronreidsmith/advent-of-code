package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day12Test extends BaseTest {
  private val input = using("2019/day12.txt")(Day12.parseInput)

  "Day12.part1" should "work on example input" in {
    Day12.part1(input) shouldBe 179
  }

  "Day12.part2" should "work on example input" in {
    Day12.part2(input) shouldBe 2772L
  }
}

package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day12Test extends BaseTest {
  private val input = using("2021/day12.txt")(Day12.parseInput)

  "Day12.part1" should "work on example input" in {
    Day12.part1(input) shouldBe 10
  }

  "Day12.part2" should "work on example input" in {
    Day12.part2(input) shouldBe 36
  }
}

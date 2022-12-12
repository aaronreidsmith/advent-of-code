package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day12Test extends BaseTest {
  private val input = using("2018/day12.txt")(Day12.parseInput)

  "Day12.part1" should "work on actual input" in {
    Day12.part1(input) shouldBe 4217
  }

  "Day12.part2" should "work on actual input" in {
    Day12.part2(input) shouldBe 4550000002111L
  }
}

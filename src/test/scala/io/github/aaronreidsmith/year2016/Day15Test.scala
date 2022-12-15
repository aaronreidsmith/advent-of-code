package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day15Test extends BaseTest {
  private val input = using("2016/day15.txt")(Day15.parseInput)

  "Day15.part1" should "work on example input" in {
    Day15.part1(input) shouldBe 5
  }

  "Day15.part2" should "work on example input" in {
    Day15.part2(input) shouldBe 85
  }
}

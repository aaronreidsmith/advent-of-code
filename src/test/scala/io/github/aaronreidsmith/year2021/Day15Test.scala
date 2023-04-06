package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day15Test extends BaseTest {
  private val input = using("2021/day15.txt")(Day15.parseInput)

  "Day15.part1" should "work on example input" in {
    Day15.part1(input) shouldBe 40
  }

  "Day15.part2" should "work on example input" in {
    Day15.part2(input) shouldBe 315
  }
}

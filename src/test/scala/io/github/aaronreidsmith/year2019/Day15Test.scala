package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day15Test extends BaseTest {
  private val instructions = using("2019/day15.txt")(Day15.parseInput)

  "Day15.part1" should "work on actual input" in {
    Day15.part1(instructions) shouldBe 252
  }

  "Day15.part" should "work on actual input" in {
    Day15.part2(instructions) shouldBe 350
  }
}

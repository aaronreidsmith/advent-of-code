package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day23Test extends BaseTest {
  private val instructions = using("2015/day23.txt")(Day23.parseInput)

  "Day23.part1" should "work on actual input" in {
    Day23.part1(instructions) shouldBe 255
  }

  "Day23.part2" should "work on actual input" in {
    Day23.part2(instructions) shouldBe 334
  }
}

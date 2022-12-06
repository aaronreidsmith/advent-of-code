package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day24Test extends BaseTest {
  private val input = using("2016/day24.txt")(Day24.parseInput)

  "Day24.part1" should "work on actual input" in {
    Day24.part1(input) shouldBe 460
  }

  "Day24.part2" should "work on actual input" in {
    Day24.part2(input) shouldBe 668
  }
}

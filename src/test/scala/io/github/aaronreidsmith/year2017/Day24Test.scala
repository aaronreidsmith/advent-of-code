package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, using}

class Day24Test extends BaseTest {
  private val input = using("2017/day24.txt")(Day24.parseInput)

  "Day24.part1" should "work on example input" in {
    Day24.part1(input) shouldBe 31
  }

  "Day24.part2" should "work on example input" in {
    Day24.part2(input) shouldBe 19
  }
}

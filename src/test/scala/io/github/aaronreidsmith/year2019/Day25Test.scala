package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day25Test extends BaseTest {
  private val input = using("2019/day25.txt")(Day25.parseInput)

  "Day25.part1" should "work on actual input" in {
    Day25.part1(input) shouldBe 84410376
  }
}

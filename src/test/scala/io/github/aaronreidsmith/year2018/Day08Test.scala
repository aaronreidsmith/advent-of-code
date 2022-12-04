package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day08Test extends BaseTest {
  private val input = using("2018/day08.txt")(Day08.parseInput)

  "Day08.part1" should "work on example input" in {
    Day08.part1(input) shouldBe 138
  }

  "Day08.part2" should "work on example input" in {
    Day08.part2(input) shouldBe 66
  }
}
package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day14Test extends BaseTest {
  private val input = using("2019/day14.txt")(Day14.parseInput)

  "Day14.part1" should "work on example input" in {
    Day14.part1(input) shouldBe 2210736L
  }

  "Day14.part2" should "work on example input" in {
    Day14.part2(input) shouldBe 460664L
  }
}

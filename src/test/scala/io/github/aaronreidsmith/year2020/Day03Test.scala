package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day03Test extends BaseTest {
  private val input = using("2020/day03.txt")(Day03.parseInput)

  "Day03.part1" should "work on example input" in {
    Day03.part1(input) shouldBe 7
  }

  "Day03.part2" should "work on example input" in {
    Day03.part2(input) shouldBe 336
  }
}

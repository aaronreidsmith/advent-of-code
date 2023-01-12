package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day11Test extends BaseTest {
  private val input = using("2020/day11.txt")(Day11.parseInput)

  "Day11.part1" should "work on example input" in {
    Day11.part1(input) shouldBe 37
  }

  "Day11.part2" should "work on example input" in {
    Day11.part2(input) shouldBe 26
  }
}

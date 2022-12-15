package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day11Test extends BaseTest {
  private val input = using("2018/day11.txt")(Day11.parseInput)

  "Day11.part1" should "work on actual input" in {
    Day11.part1(input) shouldBe "21,54"
  }

  "Day11.part2" should "work on actual input" in {
    Day11.part2(input) shouldBe "236,268,11"
  }
}

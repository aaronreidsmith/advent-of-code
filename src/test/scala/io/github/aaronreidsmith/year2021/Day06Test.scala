package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day06Test extends BaseTest {
  private val input = using("2021/day06.txt")(Day06.parseInput)

  "Day06.part1" should "work on example input" in {
    Day06.part1(input) shouldBe 5934
  }

  "Day06.part2" should "work on example input" in {
    Day06.part2(input) shouldBe 26984457539L
  }
}

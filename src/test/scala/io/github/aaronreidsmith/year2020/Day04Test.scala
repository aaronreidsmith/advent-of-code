package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day04Test extends BaseTest {
  private val input = using("2020/day04.txt")(Day04.parseInput)

  "Day04.part1" should "work on actual input" in {
    Day04.part1(input) shouldBe 237
  }

  "Day04.part2" should "work on actual input" in {
    Day04.part2(input) shouldBe 172
  }
}

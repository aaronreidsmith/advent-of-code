package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day04Test extends BaseTest {
  private val input = using("2019/day04.txt")(Day04.parseInput)

  "Day04.part1" should "work on actual input" in {
    Day04.part1(input) shouldBe 1665
  }

  "Day04.part2" should "work on actual input" in {
    Day04.part2(input) shouldBe 1131
  }
}

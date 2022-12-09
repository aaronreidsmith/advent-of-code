package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day01Test extends BaseTest {
  private val input = using("2022/day01.txt")(Day01.parseInput)

  "Day01.part1" should "work on example input" in {
    Day01.part1(input) shouldBe 24000
  }

  "Day01.part2" should "work on example input" in {
    Day01.part2(input) shouldBe 45000
  }
}

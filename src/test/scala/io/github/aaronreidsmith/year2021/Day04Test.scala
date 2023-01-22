package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day04Test extends BaseTest {
  private val input = using("2021/day04.txt")(Day04.parseInput)

  "Day04.part1" should "work on example input" in {
    Day04.part1(input) shouldBe 4512
  }

  "Day04.part2" should "work on example input" in {
    Day04.part2(input) shouldBe 1924
  }
}

package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day20Test extends BaseTest {
  private val input = using("2022/day20.txt")(Day20.parseInput)

  "Day20.part1" should "work on example input" in {
    Day20.part1(input) shouldBe 3L
  }

  "Day20.part2" should "work on example input" in {
    Day20.part2(input) shouldBe 1623178306L
  }
}

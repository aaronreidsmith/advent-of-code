package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day24Test extends BaseTest {
  private val input = using("2020/day24.txt")(Day24.parseInput)

  "Day24.part1" should "work on example input" in {
    Day24.part1(input) shouldBe 10
  }

  "Day24.part2" should "work on example input" in {
    Day24.part2(input) shouldBe 2208
  }
}

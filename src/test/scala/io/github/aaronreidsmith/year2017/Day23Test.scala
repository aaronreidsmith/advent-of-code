package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, using}

class Day23Test extends BaseTest {
  private val input = using("2017/day23.txt")(Day23.parseInput)

  "Day23.part1" should "work on example input" in {
    Day23.part1(input) shouldBe 6724
  }

  "Day23.part2" should "work on example input" in {
    Day23.part2(input) shouldBe 903
  }
}

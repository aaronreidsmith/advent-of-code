package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day18Test extends BaseTest {
  private val input = using("2021/day18.txt")(Day18.parseInput)

  "Day18.part1" should "work on example input" in {
    Day18.part1(input) shouldBe 4140
  }

  "Day18.part2" should "work on example input" in {
    Day18.part2(input) shouldBe 3993
  }
}

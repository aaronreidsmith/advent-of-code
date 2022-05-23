package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day10Test extends BaseTest {
  private val input = using("2019/day10.txt")(Day10.parseInput)

  "Day10.part1" should "work for actual input" in {
    Day10.part1(input) shouldBe 221
  }

  "Day10.part2" should "work for actual input" in {
    Day10.part2(input) shouldBe 806
  }
}

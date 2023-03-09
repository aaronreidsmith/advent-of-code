package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day10Test extends BaseTest {
  private val input = using("2021/day10.txt")(Day10.parseInput)

  "Day10.part1" should "work on example input" in {
    Day10.part1(input) shouldBe 26397
  }

  "Day10.part2" should "work on example input" in {
    Day10.part2(input) shouldBe 288957L
  }
}

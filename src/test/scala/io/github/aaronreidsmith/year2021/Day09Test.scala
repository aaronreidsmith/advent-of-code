package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day09Test extends BaseTest {
  private val input = using("2021/day09.txt")(Day09.parseInput)

  "Day09.part1" should "work on example input" in {
    Day09.part1(input) shouldBe 15
  }

  "Day09.part2" should "work on example input" in {
    Day09.part2(input) shouldBe 1134
  }
}

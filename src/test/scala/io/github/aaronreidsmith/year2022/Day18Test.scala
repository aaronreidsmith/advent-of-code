package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day18Test extends BaseTest {
  private val input = using("2022/day18.txt")(Day18.parseInput)

  "Day18.part1" should "work one example input" in {
    Day18.part1(input) shouldBe 64
  }

  "Day18.part2" should "work one example input" in {
    Day18.part2(input) shouldBe 58
  }
}

package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day17Test extends BaseTest {
  private val input = using("2018/day17.txt")(Day17.parseInput)

  "Day17.part1" should "work on example input" in {
    Day17.part1(input) shouldBe 57
  }

  "Day17.part2" should "work on example input" in {
    Day17.part2(input) shouldBe 29
  }
}

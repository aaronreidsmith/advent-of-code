package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day25Test extends BaseTest {
  private val input = using("2022/day25.txt")(Day25.parseInput)

  "Day25.part1" should "work on example input" in {
    Day25.part1(input) shouldBe "2=-1=0"
  }
}

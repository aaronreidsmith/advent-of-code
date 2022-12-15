package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day19Test extends BaseTest {
  private val input  = using("2018/day19.txt")(Day19.parseInput)

  "Day19.part1" should "work on actual input" in {
    Day19.part1(input) shouldBe 878
  }

  "Day19.part2" should "work on actual input" in {
    Day19.part2(input) shouldBe 11510496L
  }
}

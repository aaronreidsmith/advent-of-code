package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day21Test extends BaseTest {
  private val input = using("2019/day21.txt")(Day21.parseInput)

  "Day21.part1" should "work on actual input" in {
    Day21.part1(input) shouldBe 19_361_850L
  }

  "Day21.part2" should "work on actual input" in {
    Day21.part2(input) shouldBe 1_138_943_788L
  }
}

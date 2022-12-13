package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day21Test extends BaseTest {
  private val input = using("2016/day21.txt")(Day21.parseInput)

  "Day21.part1" should "work on actual input" in {
    Day21.part1(input) shouldBe "fdhbcgea"
  }

  "Day21.part2" should "work on actual input" in {
    Day21.part2(input) shouldBe "egfbcadh"
  }
}
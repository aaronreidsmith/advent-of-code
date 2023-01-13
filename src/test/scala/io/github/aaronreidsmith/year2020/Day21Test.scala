package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day21Test extends BaseTest {
  private val input = using("2020/day21.txt")(Day21.parseInput)

  "Day21.part1" should "work on example input" in {
    Day21.part1(input) shouldBe 5
  }

  "Day21.part2" should "work on example input" in {
    Day21.part2(input) shouldBe "mxmxvkd,sqjhc,fvjkl"
  }
}

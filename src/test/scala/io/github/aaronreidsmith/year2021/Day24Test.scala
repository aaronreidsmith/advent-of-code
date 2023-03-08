package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day24Test extends BaseTest {
  private val input = using("2021/day24.txt")(Day24.parseInput)

  "Day24.part1" should "work on actual input" in {
    Day24.part1(input) shouldBe "95299897999897"
  }

  "Day24.part2" should "work on actual input" in {
    Day24.part2(input) shouldBe "31111121382151"
  }
}

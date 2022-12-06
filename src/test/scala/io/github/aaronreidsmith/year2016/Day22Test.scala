package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day22Test extends BaseTest {
  private val input = using("2016/day22.txt")(Day22.parseInput)

  "Day22.part1" should "work on actual input" in {
    Day22.part1(input) shouldBe 934
  }

  "Day22.part2" should "work on actual input" in {
    Day22.part2(input) shouldBe 207
  }
}

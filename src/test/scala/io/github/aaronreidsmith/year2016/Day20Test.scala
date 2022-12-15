package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day20Test extends BaseTest {
  private val input = using("2016/day20.txt")(Day20.parseInput)

  "Day20.part1" should "work on actual input" in {
    Day20.part1(input) shouldBe 23923783L
  }

  "Day20.part2" should "work on actual input" in {
    Day20.part2(input) shouldBe 125L
  }
}

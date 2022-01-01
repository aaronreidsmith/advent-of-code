package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day16Test extends BaseTest {
  // This question doesn't have example input, so using real input
  private val sues = using("2015/day16.txt")(Day16.parseInput)

  "Day16.part1" should "work on actual input" in {
    Day16.part1(sues) shouldBe 213
  }

  "Day16.part2" should "work on actual input" in {
    Day16.part2(sues) shouldBe 323
  }
}

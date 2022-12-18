package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day05Test extends BaseTest {
  private val input = using("2019/day05.txt")(Day05.parseInput)

  "Day05.part1" should "work on actual input" in {
    Day05.part1(input) shouldBe 13294380L
  }

  "Day05.part2" should "work on actual input" in {
    Day05.part2(input) shouldBe 11460760L
  }
}

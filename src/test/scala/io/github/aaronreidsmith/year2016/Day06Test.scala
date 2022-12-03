package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day06Test extends BaseTest {
  private val input = using("2016/day06.txt")(Day06.parseInput)

  "Day06.part1" should "work on example input" in {
    Day06.part1(input) shouldBe "easter"
  }

  "Day06.part2" should "work on example input" in {
    Day06.part2(input) shouldBe "advent"
  }
}

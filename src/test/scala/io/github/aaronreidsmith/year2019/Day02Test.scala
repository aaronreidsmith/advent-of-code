package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day02Test extends BaseTest {
  private val input = using("2019/day02.txt")(Day02.parseInput)

  "Day02.part1" should "work on actual input" in {
    Day02.part1(input) shouldBe 5866663
  }

  "Day02.part2" should "work on actual input" in {
    Day02.part2(input) shouldBe 4259
  }
}

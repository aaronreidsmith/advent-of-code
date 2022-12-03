package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day02Test extends BaseTest {
  private val input = using("2016/day02.txt")(Day02.parseInput)

  "Day02.part1" should "work on example input" in {
    Day02.part1(input) shouldBe "1985"
  }

  "Day02.part2" should "work on example input" in {
    Day02.part2(input) shouldBe "5DB3"
  }
}

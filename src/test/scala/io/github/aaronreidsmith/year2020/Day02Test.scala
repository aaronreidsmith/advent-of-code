package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day02Test extends BaseTest {
  private val input = using("2020/day02.txt")(Day02.parseInput)

  "Day02.part1" should "work on example input" in {
    Day02.part1(input) shouldBe 2
  }

  "Day02.part2" should "work on example input" in {
    Day02.part2(input) shouldBe 1
  }
}
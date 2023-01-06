package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day23Test extends BaseTest {
  private val input = using("2019/day23.txt")(Day23.parseInput)

  "Day23.part1" should "work on actual input" in {
    Day23.part1(input) shouldBe 15416L
  }

  "Day23.part2" should "work on actual input" in {
    Day23.part2(input) shouldBe 10946L
  }
}

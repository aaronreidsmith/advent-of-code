package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day17Test extends BaseTest {
  private val input = using("2019/day17.txt")(Day17.parseInput)

  "Day17.part1" should "work on actual input" in {
    Day17.part1(input) shouldBe 10064
  }

  "Day17.part2" should "work on actual input" in {
    Day17.part2(input) shouldBe 1197725L
  }
}

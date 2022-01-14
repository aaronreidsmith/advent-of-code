package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day06Test extends BaseTest {
  private val input = using("2018/day06.txt")(Day06.parseInput)

  "Day06.part1" should "work for actual input" in {
    Day06.part1(input) shouldBe 5626
  }

  "Day06.part2" should "work for actual input" in {
    Day06.part2(input) shouldBe 46554
  }
}

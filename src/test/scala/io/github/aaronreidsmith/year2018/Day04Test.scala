package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day04Test extends BaseTest {
  private val input = using("2018/day04.txt")(Day04.parseInput)

  "Day04.part1" should "work for example input" in {
    Day04.part1(input) shouldBe 240
  }

  "Day04.part2" should "work on example input" in {
    Day04.part2(input) shouldBe 4455
  }
}

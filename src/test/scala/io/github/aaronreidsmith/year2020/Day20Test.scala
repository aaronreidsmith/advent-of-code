package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day20Test extends BaseTest {
  private val input = using("2020/day20.txt")(Day20.parseInput)

  "Day20.part1" should "work on example input" in {
    Day20.part1(input) shouldBe 20899048083289L
  }

  "Day20.part2" should "work on example input" in {
    Day20.part2(input) shouldBe 273
  }
}

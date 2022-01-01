package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day15Test extends BaseTest {
  private val choices = using("2015/day15.txt")(Day15.parseInput)

  "Day15.part1" should "work on example input" in {
    Day15.part1(choices) shouldBe 62842880
  }

  "Day15.part2" should "work on example input" in {
    Day15.part2(choices) shouldBe 57600000
  }
}

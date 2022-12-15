package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day17Test extends BaseTest {
  private val containers = using("2015/day17.txt")(Day17.parseInput)

  "Day17.part1" should "work on example input" in {
    Day17.part1(containers) shouldBe 4
  }

  "Day17.part2" should "work on example input" in {
    Day17.part2(containers) shouldBe 3
  }
}

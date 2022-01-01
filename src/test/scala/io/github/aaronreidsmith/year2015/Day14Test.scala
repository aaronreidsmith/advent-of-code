package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day14Test extends BaseTest {
  private val reindeer = using("2015/day14.txt")(Day14.parseInput)

  "Day14.part1" should "work on example input" in {
    Day14.part1(reindeer, 1000) shouldBe 1120
  }

  "Day14.part2" should "work on example input" in {
    Day14.part2(reindeer, 1000) shouldBe 689
  }
}

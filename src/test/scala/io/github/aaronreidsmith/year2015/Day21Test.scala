package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day21Test extends BaseTest {
  private val boss = using("2015/day21.txt")(Day21.parseInput)

  "Day21.part1" should "work on actual input" in {
    Day21.part1(boss) shouldBe 91
  }

  "Day21.part2" should "work on actual input" in {
    Day21.part2(boss) shouldBe 158
  }
}

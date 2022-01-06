package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day22Test extends BaseTest {
  private val initialGameState = using("2015/day22.txt")(Day22.parseInput)

  "Day22.part1" should "work on actual input" in {
    Day22.part1(initialGameState) shouldBe 900
  }

  "Day22.part2" should "work on actual input" in {
    Day22.part2(initialGameState) shouldBe 1216
  }
}

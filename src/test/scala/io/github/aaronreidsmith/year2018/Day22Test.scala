package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day22Test extends BaseTest {
  private val (grid, target, corner) = using("2018/day22.txt")(Day22.parseInput)

  "Day22.part1" should "work on actual input" in {
    Day22.part1(grid, target) shouldBe 6323
  }

  "Day22.part2" should "work on actual input" in {
    Day22.part2(grid, corner, target) shouldBe 982
  }
}

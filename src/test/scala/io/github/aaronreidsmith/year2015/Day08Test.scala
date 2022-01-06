package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day08Test extends BaseTest {
  private val input = using("2015/day08.txt")(_.getLines().toList)

  "Day08.part1" should "work on example input" in {
    Day08.part1(input) shouldBe 12
  }

  "Day08.part2" should "work on example input" in {
    Day08.part2(input) shouldBe 19
  }
}

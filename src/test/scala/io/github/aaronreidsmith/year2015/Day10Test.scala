package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day10Test extends BaseTest {
  private val input = 1321131112 // TODO: My solution doesn't work on short input, so I just use the real input to test

  "Day10.part1" should "work on actual input" in {
    Day10.part1(input) shouldBe 492982
  }

  "Day10.part2" should "work on actual input" in {
    Day10.part2(input) shouldBe 6989950
  }
}

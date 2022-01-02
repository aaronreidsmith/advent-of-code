package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day25Test extends BaseTest {
  "Day25.part1" should "work on actual input" in {
    Day25.part1(2978, 3083) shouldBe 2650453L
  }
}

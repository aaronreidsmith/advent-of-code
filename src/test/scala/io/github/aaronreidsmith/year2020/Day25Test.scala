package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest

class Day25Test extends BaseTest {
  "Day25.part1" should "work on example input" in {
    val input = (5764801L, 17807724L)
    Day25.part1(input) shouldBe 14897079L
  }
}

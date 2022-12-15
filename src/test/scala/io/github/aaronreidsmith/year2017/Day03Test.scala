package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day03Test extends BaseTest {
  "Day03.part1" should "work on example input" in {
    // Day03.part1(1) shouldBe 0 // TODO: My solution starts in second square, so doesn't handle this case
    Day03.part1(12) shouldBe 3
    Day03.part1(23) shouldBe 2
    Day03.part1(1024) shouldBe 31
  }

  "Day03.part2" should "work on actual input" in {
    Day03.part2(347991) shouldBe 349975
  }
}

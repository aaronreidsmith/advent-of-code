package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day24Test extends BaseTest {
  private val gifts = using("2015/day24.txt")(Day24.parseInput)

  "Day24.part1" should "work on actual input" in {
    Day24.part1(gifts) shouldBe BigInt(10439961859L)
  }

  "Day24.part2" should "work on actual input" in {
    Day24.part2(gifts) shouldBe BigInt(72050269)
  }
}

package io.github.aaronreidsmith.year2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
  private val input = 1321131112 // My solution doesn't work on short input, so I just use the real input to test

  "Day10.part1" should "work on actual input" in {
    Day10.part1(input) shouldBe 492982
  }

  "Day10.part2" should "work on actual input" in {
    Day10.part2(input) shouldBe 6989950
  }
}

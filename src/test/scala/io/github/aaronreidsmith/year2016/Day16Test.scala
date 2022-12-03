package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import org.scalatest.tagobjects.Slow

class Day16Test extends BaseTest {
  private val input = "10001110011110000"

  "Day16.part1" should "work on actual input" in {
    Day16.part1(input) shouldBe "10010101010011101"
  }

  "Day16.part2" should "work on actual input" taggedAs Slow in {
    Day16.part2(input) shouldBe "01100111101101111"
  }
}

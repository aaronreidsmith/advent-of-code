package io.github.aaronreidsmith.year2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  private val input = "hxbxwxba" // No example input for this problem

  "Day11.part1" should "work on actual input" in {
    Day11.part1(input) shouldBe "hxbxxyzz"
  }

  "Day11.part2" should "work on actual input" in {
    Day11.part2(input) shouldBe "hxcaabcc"
  }
}

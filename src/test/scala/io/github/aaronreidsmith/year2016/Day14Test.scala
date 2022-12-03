package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import org.scalatest.tags.Slow

@Slow
class Day14Test extends BaseTest {
  private val input = "abc"

  "Day14.part1" should "work on example input" in {
    Day14.part1(input) shouldBe 22728
  }

  "Day14.part2" should "work on example input" in {
    Day14.part2(input) shouldBe 22551
  }
}

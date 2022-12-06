package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tags.Slow

@Slow
class Day25Test extends BaseTest {
  private val input = using("2016/day25.txt")(Day25.parseInput)

  "Day25.part1" should "work on actual input" in {
    Day25.part1(input) shouldBe 158
  }
}

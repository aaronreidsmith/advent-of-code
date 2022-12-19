package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tagobjects.Slow

class Day19Test extends BaseTest {
  private val input = using("2022/day19.txt")(Day19.parseInput)

  "Day19.part1" should "work on actual input" in {
    Day19.part1(input) shouldBe 1127
  }

  "Day19.part2" should "work on actual input" taggedAs Slow in {
    Day19.part2(input) shouldBe 21546
  }
}

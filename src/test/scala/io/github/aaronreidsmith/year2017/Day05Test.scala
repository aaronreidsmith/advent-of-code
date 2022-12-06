package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tagobjects.Slow

class Day05Test extends BaseTest {
  private val input = using("2017/day05.txt")(Day05.parseInput)

  "Day05.part1" should "work on actual input" in {
    Day05.part1(input) shouldBe 355965
  }

  "Day05.part2" should "work on actual input" taggedAs Slow in {
    Day05.part2(input) shouldBe 26948068
  }
}

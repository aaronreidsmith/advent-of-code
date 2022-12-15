package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tagobjects.Slow

class Day18Test extends BaseTest {
  private val input = using("2016/day18.txt")(Day18.parseInput)

  "Day18.part1" should "work on actual input" in {
    Day18.part1(input) shouldBe 1961
  }

  "Day18.part2" should "work on actual input" taggedAs Slow in {
    Day18.part2(input) shouldBe 20000795
  }
}

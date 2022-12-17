package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tags.Slow

@Slow
class Day15Test extends BaseTest {
  private val input = using("2018/day15.txt")(Day15.parseInput)

  "Day15.part1" should "work on actual input" in {
    Day15.part1(input) shouldBe 231264
  }

  "Day15.part2" should "work on actual input" in {
    Day15.part2(input) shouldBe 42224
  }
}

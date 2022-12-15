package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tags.Slow

@Slow
class Day12Test extends BaseTest {
  private val input = using("2016/day12.txt")(Day12.parseInput)

  "Day12.part1" should "work on actual input" in {
    Day12.part1(input) shouldBe 318009
  }

  "Day12.part2" should "work on actual input" in {
    Day12.part2(input) shouldBe 9227663
  }
}

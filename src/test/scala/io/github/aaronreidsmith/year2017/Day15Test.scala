package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, IgnoreOnCI, using}
import org.scalatest.tags.Slow

@Slow
@IgnoreOnCI
class Day15Test extends BaseTest {
  private val input = using("2017/day15.txt")(Day15.parseInput)

  "Day15.part1" should "work on example input" in {
    Day15.part1(input) shouldBe 588
  }

  "Day15.part2" should "work on example input" in {
    Day15.part2(input) shouldBe 285
  }
}

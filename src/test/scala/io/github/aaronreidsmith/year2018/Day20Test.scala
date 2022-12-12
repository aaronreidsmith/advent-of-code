package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, IgnoreOnCI, using}
import org.scalatest.tags.Slow

@Slow
@IgnoreOnCI
class Day20Test extends BaseTest {
  private val input = using("2018/day20.txt")(Day20.parseInput)

  "Day20.part1" should "work on actual input" in {
    Day20.part1(input) shouldBe 3872
  }

  "Day20.part2" should "work on actual input" in {
    Day20.part2(input) shouldBe 8600
  }
}

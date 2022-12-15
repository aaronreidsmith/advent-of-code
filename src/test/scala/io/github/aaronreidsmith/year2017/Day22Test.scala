package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tagobjects.Slow

class Day22Test extends BaseTest {
  private val input = using("2017/day22.txt")(Day22.parseInput)

  "Day22.part1" should "work on example input" in {
    Day22.part1(input) shouldBe 5587
  }

  "Day22.part2" should "work on example input" taggedAs Slow in {
    Day22.part2(input) shouldBe 2511944
  }
}

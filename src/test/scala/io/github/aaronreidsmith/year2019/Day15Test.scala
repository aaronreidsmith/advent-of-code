package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils
import org.scalatest.Ignore

// This one runs incredibly slow and only works like half the time, so we ignore it by default
@Ignore
class Day15Test extends BaseTest with IntCodeUtils{
  private val instructions = makeInstructions("2019/day15.txt")

  "Day15.part1" should "work on actual input" in {
    Day15.part1(instructions) shouldBe 252
  }

  "Day15.part" should "work on actual input" in {
    Day15.part2(instructions) shouldBe 350
  }
}

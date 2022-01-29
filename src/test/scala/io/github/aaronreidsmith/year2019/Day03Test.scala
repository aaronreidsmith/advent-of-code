package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day03Test extends BaseTest {
  private val wires = using("2019/day03.txt")(Day03.parseInput)

  "Day03.part1" should "work on example input" in {
    Day03.part1(wires) shouldBe 159
  }

  "Day03.part2" should "work on example input" in {
    Day03.part2(wires) shouldBe 610
  }
}

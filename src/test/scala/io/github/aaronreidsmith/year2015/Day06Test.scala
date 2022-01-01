package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day06Test extends BaseTest {
  "Day06.part1" should "work on example input" in {
    val instructions = List(
      "turn on 0,0 through 999,999",     // turn on 1M lights
      "toggle 0,0 through 999,0",        // Turn off 1000 lights
      "turn off 499,499 through 500,500" // Turn of 4 lights
    )
    Day06.part1(instructions) shouldBe (1000000 - 1000 - 4)
  }

  "Day06.part2" should "work on example input" in {
    val instructions = List(
      "turn on 0,0 through 0,0",   // Increase brightness by 1
      "toggle 0,0 through 999,999" // Increase brightness by 2M
    )
    Day06.part2(instructions) shouldBe 2000001
  }
}

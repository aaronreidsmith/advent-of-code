package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day06Test extends BaseTest {
  "Day06.part1" should "work on example input" in {
    // Turn on 1M lights, then turn off 1000 lights, then turn off 4 lights
    val source = Source.fromString(
      """turn on 0,0 through 999,999
        |toggle 0,0 through 999,0
        |turn off 499,499 through 500,500""".stripMargin
    )
    val instructions = Day06.parseInput(source)
    Day06.part1(instructions) shouldBe (1000000 - 1000 - 4)
  }

  "Day06.part2" should "work on example input" in {
    // Increase brightness by 1 the increase brightness by 2M
    val source = Source.fromString(
      """turn on 0,0 through 0,0
        |toggle 0,0 through 999,999""".stripMargin
    )
    val instructions = Day06.parseInput(source)
    Day06.part2(instructions) shouldBe 2000001
  }
}

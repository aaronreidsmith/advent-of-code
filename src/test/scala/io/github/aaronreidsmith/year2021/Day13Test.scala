package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{BaseTest, using}

class Day13Test extends BaseTest {
  private val input = using("2021/day13.txt")(Day13.parseInput)

  "Day13.part1" should "work on example input" in {
    Day13.part1(input) shouldBe 17
  }

  "Day13.part2" should "work on example input" in {
    Day13.part2(input) shouldBe """
                                  |#####
                                  |#   #
                                  |#   #
                                  |#   #
                                  |#####
                                  |""".stripMargin
  }
}

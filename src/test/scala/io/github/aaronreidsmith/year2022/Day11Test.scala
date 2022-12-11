package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day11Test extends BaseTest {
  private val input = using("2022/day11.txt")(Day11.parseInput)

  "Day11" should "work on example input" in {
    Day11.part1(input) shouldBe 10605L
    input._1.values.foreach(_.reset()) // Make sure we test our rest
    Day11.part2(input) shouldBe 2713310158L
  }
}

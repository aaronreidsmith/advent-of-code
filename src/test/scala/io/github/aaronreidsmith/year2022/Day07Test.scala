package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day07Test extends BaseTest {
  private val input = using("2022/day07.txt")(Day07.parseInput)

  "Day07.part1" should "work on example input" in {
    Day07.part1(input) shouldBe 95437
  }

  "Day07.part2" should "work on example input" in {
    Day07.part2(input) shouldBe 24933642
  }
}

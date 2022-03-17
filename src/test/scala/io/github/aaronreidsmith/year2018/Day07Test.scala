package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day07Test extends BaseTest {
  private val input = using("2018/day07.txt")(Day07.parseInput)

  "Day07.part1" should "work for actual input" in {
    Day07.part1(input) shouldBe "HEGMPOAWBFCDITVXYZRKUQNSLJ"
  }

  "Day07.part2" should "work for actual input" in {
    Day07.part2(input) shouldBe 1226
  }
}

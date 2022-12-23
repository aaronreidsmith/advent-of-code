package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

// Example input works on part 1, but didn't wanna code part 2 to handle arbitrary cubes, so it only handles my input
class Day22Test extends BaseTest {
  private val input = using("2022/day22.txt")(Day22.parseInput)

  "Day22.part1" should "work on actual input" in {
    Day22.part1(input) shouldBe 190066
  }

  "Day22.part2" should "work on actual input" in {
    Day22.part2(input) shouldBe 134170
  }
}

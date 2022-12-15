package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, using}

class Day16Test extends BaseTest {
  private val input = using("2017/day16.txt")(Day16.parseInput)

  "Day16.part1" should "work on actual input" in {
    Day16.part1(input) shouldBe "lbdiomkhgcjanefp"
  }

  "Day16.part2" should "work on actual input" in {
    Day16.part2(input) shouldBe "ejkflpgnamhdcboi"
  }
}

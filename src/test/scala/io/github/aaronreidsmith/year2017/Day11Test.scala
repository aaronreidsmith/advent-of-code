package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day11Test extends BaseTest {
  "Day11" should "work on example input" in {
    Seq(
      ("ne,ne,ne", 3, 3),
      ("ne,ne,sw,sw", 0, 3),
      ("ne,ne,s,s", 2, 3),
      ("se,sw,se,sw,sw", 3, 3)
    ).foreach {
      case (input, expected1, expected2) =>
        val parsed = Day11.parseInput(Source.fromString(input))
        Day11.part1(parsed) shouldBe expected1
        Day11.part2(parsed) shouldBe expected2
    }
  }
}

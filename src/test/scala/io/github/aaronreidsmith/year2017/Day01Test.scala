package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day01Test extends BaseTest {
  "Day01.part1" should "work on example input" in {
    Seq(
      ("1122", 3),
      ("1111", 4),
      ("1234", 0),
      ("91212129", 9)
    ).foreach {
      case (input, expected) =>
        val parsed = Day01.parseInput(input.asSource)
        Day01.part1(parsed) shouldBe expected
    }
  }

  "Day01.part2" should "work on example input" in {
    Seq(
      ("1212", 6),
      ("1221", 0),
      ("123424", 4),
      ("123123", 12),
      ("12131415", 4)
    ).foreach {
      case (input, expected) =>
        val parsed = Day01.parseInput(input.asSource)
        Day01.part2(parsed) shouldBe expected
    }
  }
}

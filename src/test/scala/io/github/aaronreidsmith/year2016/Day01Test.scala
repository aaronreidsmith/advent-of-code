package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day01Test extends BaseTest {
  "Day01.part1" should "work on example input" in {
    Seq(
      ("R2, L3", 5),
      ("R2, R2, R2", 2),
      ("R5, L5, R5, R3", 12)
    ).foreach {
      case (inputString, expected) =>
        val input = Day01.parseInput(inputString.asSource)
        Day01.part1(input) shouldBe expected
      case _ => // Do nothing
    }
  }

  "Day01.part2" should "work on example input" in {
    val inputString = "R8, R4, R4, R8"
    val input       = Day01.parseInput(inputString.asSource)
    Day01.part2(input) shouldBe 4
  }
}

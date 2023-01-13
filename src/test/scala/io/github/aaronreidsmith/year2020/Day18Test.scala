package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest

class Day18Test extends BaseTest {
  "Day18" should "work on example input" in {
    Seq(
      ("1 + 2 * 3 + 4 * 5 + 6", 71, 231),
      ("1 + (2 * 3) + (4 * (5 + 6))", 51, 51),
      ("2 * 3 + (4 * 5)", 26, 46),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437, 1445),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240, 669060),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632, 23340)
    ).foreach {
      case (input, expected1, expected2) =>
        val parsed = Day18.parseInput(input.asSource)
        Day18.part1(parsed) shouldBe expected1
        Day18.part2(parsed) shouldBe expected2
    }
  }
}

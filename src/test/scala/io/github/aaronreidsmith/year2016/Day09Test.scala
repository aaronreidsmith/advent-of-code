package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  "Day09.part1" should "work on example input" in {
    Seq(
      ("ADVENT", 6),
      ("A(1x5)BC", 7),
      ("(3x3)XYZ", 9),
      ("A(2x2)BCD(2x2)EFG", 11),
      ("(6x1)(1x3)A", 6),
      ("X(8x2)(3x3)ABCY", 18)
    ).foreach {
      case (input, expected) => Day09.part1(input) shouldBe expected
    }
  }

  "Day09.part2" should "work on example input" in {
    Seq(
      ("(3x3)XYZ", 9L),
      ("X(8x2)(3x3)ABCY", 20L),
      ("(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920L),
      ("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445L)
    ).foreach {
      case (input, expected) => Day09.part2(input) shouldBe expected
    }
  }
}

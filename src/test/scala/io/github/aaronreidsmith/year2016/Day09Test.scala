package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("ADVENT", "A(1x5)BC", "(3x3)XYZ", "A(2x2)BCD(2x2)EFG", "(6x1)(1x3)A", "X(8x2)(3x3)ABCY"),
    Seq(6, 7, 9, 11, 6, 18),
    Seq(
      "(3x3)XYZ",
      "X(8x2)(3x3)ABCY",
      "(27x12)(20x12)(13x14)(7x10)(1x12)A",
      "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    ),
    Seq(9L, 20L, 241920L, 445L)
  )
}

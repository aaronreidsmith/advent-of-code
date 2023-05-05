package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest

class Day18Test extends BaseTest {
  val suite: Suite = Suite(
    Seq(
      "1 + 2 * 3 + 4 * 5 + 6",
      "1 + (2 * 3) + (4 * (5 + 6))",
      "2 * 3 + (4 * 5)",
      "5 + (8 * 3 + 9 + 3 * 4 * 3)",
      "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
      "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    ).parsed,
    Seq(71, 51, 26, 437, 12240, 13632),
    Seq(231, 51, 46, 1445, 669060, 23340)
  )
}

package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day02Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("2x3x4", "1x1x10").parsed,
    Seq(58, 43),
    Seq(34, 14)
  )
}

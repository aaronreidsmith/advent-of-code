package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day03Test extends BaseTest {
  val suite: Suite = Suite(
    Seq(12, 23, 1024), // TODO: My solution starts in second square, so doesn't handle case of 1 (in) -> 0 (out)
    Seq(3, 2, 31),
    Seq(347991),
    Seq(349975)
  )
}
